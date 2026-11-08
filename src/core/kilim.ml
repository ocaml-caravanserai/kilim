module Span = Span

module Private = struct
  let set_error_handler f = Report.handler := f

  let error = Report.error

  let set_warning_handler f = Report.warning_handler := f

  module Host = Host
  module Cancel = Cancel
  module Dlist = Dlist
  module Run_queue = Run_queue
  module Async = Async
  module Key = Key
  module Unsafe_html = Unsafe_html
  module Names = Names
  module View = View
  module Dom = Dom
  module Tree = Tree
  module Instance = Instance
  module Diff = Diff
  module Reconcile = Reconcile
  module Cmd = Cmd
  module Sub = Sub
  module App = App
  module Event = Event
  module Runtime = Runtime
  module Shallow = Shallow
  module Component = Component

  module type COMPONENT = Component.S
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

module Cmd = struct
  type 'msg t = 'msg Cmd.t

  let none = Cmd.None

  let batch l = Cmd.Batch l

  let task t = Cmd.Task t

  let latest ~key t = Cmd.Latest (key, t)

  let map ?scope f c = Cmd.Map (scope, f, c)
end

module Sub = struct
  type 'msg t = 'msg Sub.t

  let none = Sub.None

  let batch l = Sub.Batch l

  let every = Sub.every

  let run ~key f = Sub.Run (key, f)
end

module App = struct
  type t = App.t

  let make = App.make
end

module Component = struct
  let shallow_equal = Shallow.equal
end
