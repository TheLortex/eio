type t = {
  waiters: unit Waiters.t; 
  mutex: Mutex.t;
  id: Ctf.id
}

let create ?label () =
  let id = Ctf.mint_id () in
  Ctf.note_created ?label id Ctf.Condition;
  {
    waiters = Waiters.create ();
    id ;
    mutex = Mutex.create ()
  }

let await ?mutex t = 
  Mutex.lock t.mutex;
  Option.iter Eio_mutex.unlock mutex;
  Waiters.await ~mutex:(Some t.mutex) t.waiters t.id;
  Option.iter Eio_mutex.lock mutex

let broadcast t =
  Waiters.wake_all t.waiters ()
