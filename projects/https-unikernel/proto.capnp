@0xe81d238ec50a0daa;

struct Response {
  union {
    ok @0 :Text;
    notFound @1 :Void;
  }
}

interface Store {
  get @0 (path :List(Text)) -> (result :Response);
}
