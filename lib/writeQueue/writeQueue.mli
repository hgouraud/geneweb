(** Queue of the write requests of a base.

    A write request that cannot take the lock of its base at once is saved here
    instead of being refused, and is applied later, in the order of arrival, by
    the queue runner of gwd, without the browser of the wizard. The queue of
    [base.gwb] is the directory [base.queue] next to it, kept out of the base so
    that a rebuild of the base does not lose it:
    - [pending/<id>]: the requests to apply, [id] being their order of arrival;
    - [done/<id>]: the requests applied, kept a few days for their wizards;
    - [unseen/<user>/<id>]: the requests applied that their wizard has not been
      told of yet.

    Its files hold the identity of a wizard and its entries: the directory is
    readable by the server only. *)

type state =
  | Pending  (** waiting for its turn *)
  | Running  (** being applied; found again only after a crash *)
  | Applied  (** written into the base *)
  | Unchanged  (** applied, the base being unchanged *)
  | Review  (** not written: the wizard has to check it (conflict, choice) *)
  | Failed  (** refused by the update (error page) *)

type identity = {
  wizard : bool;
  friend : bool;
  user : string;
  username : string;
  userkey : string;
}
(** Rights of the wizard who posted the request, as authenticated then: the
    request is applied with them, even if the session has ended since. *)

type item = {
  id : int;
  created : float;
  identity : identity;
  command : string;  (** the command [m] *)
  from : string;  (** the client address *)
  lang : string;
  request : string list;
      (** the header lines of the HTTP request, without its credentials *)
  env : (string * Adef.encoded_string) list;  (** the parameters posted *)
  state : state;
  finished : float;  (** end of the application, [0.] before *)
}

val dir : string -> string
(** [dir bfile] is the queue directory of the base [bfile] ([base.gwb]). *)

val has_pending : string -> bool
(** [has_pending bfile] tells whether requests wait in the queue of [bfile].
    Cheap: reads a directory. *)

val enqueue :
  string ->
  identity:identity ->
  command:string ->
  from:string ->
  lang:string ->
  request:string list ->
  env:(string * Adef.encoded_string) list ->
  item
(** [enqueue bfile ~identity ~command ~from ~lang ~request ~env] saves a request
    at the end of the queue of [bfile] and returns it. A request equal to one
    still waiting (same wizard, command and parameters: a form posted twice) is
    not saved again, the one waiting is returned. The lines [Authorization] and
    [Cookie] of [request] are dropped. *)

val position : string -> int -> int option
(** [position bfile id] is the rank, from 1, of the request [id] among those
    waiting; [None] when it is no longer waiting. *)

val next : string -> item option
(** [next bfile] is the oldest request waiting, marked {!Running}. To be called
    while holding the lock of the base. A request found {!Running}, interrupted
    by a crash, is not applied again: it is finished as {!Review}. *)

val finish : string -> item -> state -> unit
(** [finish bfile item state] ends the request [item] with [state] and marks it
    as unseen by its wizard. *)

val find : string -> int -> item option
(** [find bfile id] is the request [id], waiting or finished. *)

val list_user : string -> user:string -> item list
(** [list_user bfile ~user] lists the requests of the wizard [user], waiting or
    finished, the most recent first. *)

val take_unseen : string -> user:string -> item list
(** [take_unseen bfile ~user] lists the requests of [user] finished since he was
    last told of them, and marks them as seen. *)

val purge : string -> max_age:float -> unit
(** [purge bfile ~max_age] removes the requests finished more than [max_age]
    seconds ago. *)
