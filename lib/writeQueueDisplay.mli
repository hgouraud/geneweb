(** Pages of the write queue ({!WriteQueue}). *)

val print_queued : Config.config -> string -> WriteQueue.item -> unit
(** [print_queued conf bfile item] prints the page telling that the request
    [item] has been queued in the queue of the base [bfile], and will be applied
    automatically. *)

val print_list : Config.config -> Geneweb_db.Driver.base -> unit
(** [print_list conf base] prints the page [m=QUEUE]: the requests of the wizard
    queued in the last days, with their state. Those to check or refused can be
    posted again, in the session of the wizard, with the button "resume". *)

val notify : Config.config -> unit
(** [notify conf] tells the wizard, by a notification, of his requests applied
    from the queue since his last page. *)
