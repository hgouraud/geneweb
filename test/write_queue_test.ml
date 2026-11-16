open Alcotest

let bfile = "write_queue_test/base.gwb"

let identity user =
  WriteQueue.
    { wizard = true; friend = false; user; username = user; userkey = "" }

let enqueue ?(user = "henri") occu =
  WriteQueue.enqueue bfile ~identity:(identity user) ~command:"MOD_IND_OK"
    ~from:"127.0.0.1" ~lang:"fr"
    ~request:
      [ "Host: localhost"; "Authorization: Basic aGVucmk6eA=="; "Cookie: s=1" ]
    ~env:[ ("i", Adef.encoded "1"); ("occu", Adef.encoded occu) ]

let state =
  testable
    (Fmt.of_to_string (fun (s : WriteQueue.state) ->
         match s with
         | Pending -> "pending"
         | Running -> "running"
         | Applied -> "applied"
         | Unchanged -> "unchanged"
         | Review -> "review"
         | Failed -> "failed"))
    ( = )

let next () =
  match WriteQueue.next bfile with Some i -> i | None -> fail "empty queue"

let test_queue () =
  (check bool) "empty" false (WriteQueue.has_pending bfile);
  let a = enqueue "meunier" in
  let b = enqueue "tisserand" in
  (check bool) "pending" true (WriteQueue.has_pending bfile);
  (check (option int)) "first" (Some 1) (WriteQueue.position bfile a.id);
  (check (option int)) "second" (Some 2) (WriteQueue.position bfile b.id);
  (check int) "posted twice" b.id (enqueue "tisserand").id;
  (check int) "other wizard" (b.id + 1) (enqueue ~user:"a2" "tisserand").id;
  (check (list string)) "credentials dropped" [ "Host: localhost" ] a.request;
  let i = next () in
  (check int) "in order" a.id i.id;
  (check state) "running" Running i.state;
  WriteQueue.finish bfile i Applied;
  (check (option int)) "finished" None (WriteQueue.position bfile a.id);
  (check (option int)) "moved up" (Some 1) (WriteQueue.position bfile b.id);
  let i = next () in
  (check int) "next one" b.id i.id;
  (* the runner stops while applying [b]: it is not applied twice *)
  let i = next () in
  (check bool) "interrupted one skipped" true (i.id <> b.id);
  (check (option state))
    "interrupted to check" (Some Review)
    (Option.map
       (fun (i : WriteQueue.item) -> i.state)
       (WriteQueue.find bfile b.id));
  WriteQueue.finish bfile i Applied;
  (check bool) "drained" false (WriteQueue.has_pending bfile);
  (check (list int))
    "list of henri" [ b.id; a.id ]
    (List.map
       (fun (i : WriteQueue.item) -> i.id)
       (WriteQueue.list_user bfile ~user:"henri"));
  (check (list int))
    "unseen" [ a.id; b.id ]
    (List.map
       (fun (i : WriteQueue.item) -> i.id)
       (WriteQueue.take_unseen bfile ~user:"henri"));
  (check int) "seen" 0
    (List.length (WriteQueue.take_unseen bfile ~user:"henri"));
  WriteQueue.purge bfile ~max_age:(-1.);
  (check int) "purged" 0
    (List.length (WriteQueue.list_user bfile ~user:"henri"))

let v = [ ("write-queue", [ test_case "queue" `Quick test_queue ]) ]
