module Span = Span

module Private = struct
  let set_error_handler f = Report.handler := f

  module Host = Host
  module Cancel = Cancel
  module Run_queue = Run_queue
  module Async = Async
  module Key = Key
  module Unsafe_html = Unsafe_html
  module Names = Names
  module View = View
  module Dom = Dom
  module Diff = Diff
end

module Cancel = Cancel
module Async = Async

module Key = struct
  type t = Key.t

  let int = Key.int

  let string = Key.string
end

module Unsafe_html = struct
  type t = Unsafe_html.t

  let of_string = Unsafe_html.of_string

  let to_string = Unsafe_html.to_string
end

module View = struct
  type 'msg t = 'msg View.t

  let txt = View.txt

  let keyed = View.keyed

  let unsafe_html = View.unsafe_html
end

let txt = View.txt

module At = At
module Ev = Ev
module El = El
