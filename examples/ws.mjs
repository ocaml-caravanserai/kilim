// A minimal WebSocket server (RFC 6455) with the built-in modules of node
// only. It is for the tests and the examples of kilim, not for production:
// - The handshake of section 4.2 (Sec-WebSocket-Accept), with no protocols
//   and no extensions.
// - Text, binary and close frames. The frames of the client must be masked,
//   and the server does not accept fragments (a frame with no FIN bit) or a
//   frame larger than 1 MiB (the code 1009). It ignores the other opcodes,
//   also ping and pong.
// - A room sends each text to all its connections (a broadcast).
//
// The caller adds the error handler of the socket:
//
//   import { accept, refuse, Room } from './ws.mjs';
//   server.on('upgrade', (req, socket) => {
//     socket.on('error', () => {});
//     if (req.url === '/echo') accept(req, socket, (c) => { c.onmessage = (t) => c.send(t); });
//     else refuse(socket);
//   });

import { createHash } from 'node:crypto';

const GUID = '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';
const MAX = 1 << 20;

// A frame of the server: FIN, the opcode, the length and no mask.
const frame = (opcode, payload) => {
  const n = payload.length;
  let head;
  if (n < 126) head = Buffer.from([0x80 | opcode, n]);
  else if (n < 65536) {
    head = Buffer.from([0x80 | opcode, 126, 0, 0]);
    head.writeUInt16BE(n, 2);
  } else {
    head = Buffer.alloc(10);
    head[0] = 0x80 | opcode;
    head[1] = 127;
    head.writeBigUInt64BE(BigInt(n), 2);
  }
  return Buffer.concat([head, payload]);
};

const closePayload = (code, reason) => {
  const r = Buffer.from(reason ?? '', 'utf8');
  const b = Buffer.alloc(2 + r.length);
  b.writeUInt16BE(code, 0);
  r.copy(b, 2);
  return b;
};

// One connection. Set onmessage (text, isBinary) and onclose (). The
// connection calls onclose one time, when the TCP connection closes.
export class Conn {
  constructor(socket) {
    this.socket = socket;
    this.closing = false;
    this.onmessage = null;
    this.onclose = null;
  }

  send(text) {
    if (!this.closing) this.socket.write(frame(1, Buffer.from(text, 'utf8')));
  }

  sendBinary(buf) {
    if (!this.closing) this.socket.write(frame(2, buf));
  }

  // The close handshake: the close frame, then the end of the TCP connection
  // after the close frame of the client.
  close(code = 1000, reason = '') {
    if (this.closing) return;
    this.closing = true;
    this.socket.write(frame(8, closePayload(code, reason)));
  }
}

// The frame in [buf] at [at], and the next offset, or null if the frame is not
// complete. A frame larger than MAX gives { tooLarge: true }.
const parse = (buf, at) => {
  if (buf.length < at + 2) return null;
  const b0 = buf[at], b1 = buf[at + 1];
  let n = b1 & 0x7f, i = at + 2;
  if (n === 126) {
    if (buf.length < i + 2) return null;
    n = buf.readUInt16BE(i);
    i += 2;
  } else if (n === 127) {
    if (buf.length < i + 8) return null;
    const big = buf.readBigUInt64BE(i);
    n = big > BigInt(MAX) ? MAX + 1 : Number(big);
    i += 8;
  }
  if (n > MAX) return { tooLarge: true };
  const masked = (b1 & 0x80) !== 0;
  const mask = masked ? buf.subarray(i, i + 4) : null;
  if (masked) i += 4;
  if (buf.length < i + n) return null;
  const payload = Buffer.from(buf.subarray(i, i + n));
  if (masked) for (let k = 0; k < n; k++) payload[k] ^= mask[k & 3];
  return { fin: (b0 & 0x80) !== 0, opcode: b0 & 0x0f, masked, payload, next: i + n };
};

// Accept the upgrade request [req] on [socket], then call [onopen] with the
// connection.
export const accept = (req, socket, onopen) => {
  const key = req.headers['sec-websocket-key'];
  if (!key || (req.headers.upgrade ?? '').toLowerCase() !== 'websocket') {
    socket.end('HTTP/1.1 400 Bad Request\r\n\r\n');
    return;
  }
  const digest = createHash('sha1').update(key + GUID).digest('base64');
  socket.write('HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\n' +
    `Connection: Upgrade\r\nSec-WebSocket-Accept: ${digest}\r\n\r\n`);
  const c = new Conn(socket);
  let buf = Buffer.alloc(0);
  socket.on('data', (data) => {
    buf = Buffer.concat([buf, data]);
    let at = 0, f;
    while ((f = parse(buf, at)) !== null) {
      if (f.tooLarge || !f.masked || !f.fin || f.opcode === 0) {
        // A frame that is too large (1009), a protocol error (1002), or a
        // fragment (1003: not accepted).
        c.close(f.tooLarge ? 1009 : f.masked ? 1003 : 1002);
        socket.end();
        at = buf.length;
        break;
      }
      at = f.next;
      if (f.opcode === 1 || f.opcode === 2) {
        if (c.onmessage) c.onmessage(f.opcode === 1 ? f.payload.toString('utf8') : f.payload, f.opcode === 2);
      } else if (f.opcode === 8) {
        // The answer to a close of the client has the same code. A payload of
        // 1 byte is not valid: the answer has no code.
        if (!c.closing) {
          c.closing = true;
          socket.write(frame(8, f.payload.length >= 2 ? f.payload.subarray(0, 2) : Buffer.alloc(0)));
        }
        socket.end();
        break;
      }
    }
    buf = buf.subarray(at);
  });
  socket.on('close', () => { if (c.onclose) c.onclose(); });
  onopen(c);
};

// Refuse the upgrade request on [socket] with the status 404.
export const refuse = (socket) => socket.end('HTTP/1.1 404 Not Found\r\n\r\n');

// A set of connections: send gives each text to all of them.
export class Room {
  constructor() {
    this.conns = new Set();
  }

  join(c) {
    this.conns.add(c);
    const onclose = c.onclose;
    c.onclose = () => {
      this.conns.delete(c);
      if (onclose) onclose();
    };
  }

  send(text) {
    for (const c of this.conns) c.send(text);
  }
}
