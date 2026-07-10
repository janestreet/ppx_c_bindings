open Core
include Ox

let sexp_of_mem n =
  [%sexp ([%c.no_alloc ({| return ((uint64_t)%{n:Ox.mem}); |} : Int64.t)] : Int64.Hex.t)]
;;

module Ptr = struct
  include Ptr

  module Ext = struct
    include Ext

    let sexp_of_t (type a : any) (_ : a -> Sexp.t) n =
      [%sexp
        ([%c.no_alloc ({| return ((uint64_t)%{n:a Ox.Ptr.Ext.t}); |} : Int64.t)]
         : Int64.Hex.t)]
    ;;

    module Imm = struct
      include Imm

      let sexp_of_t (type a : any) (_ : a -> Sexp.t) n =
        [%sexp
          ([%c.no_alloc ({| return ((uint64_t)%{n:a Ox.Ptr.Ext.Imm.t}); |} : Int64.t)]
           : Int64.Hex.t)]
      ;;
    end
  end
end

module Addr = struct
  include Addr

  module Ext = struct
    include Ext

    let sexp_of_t (type a : any) (_ : a -> Sexp.t) n =
      [%sexp
        ([%c.no_alloc ({| return ((uint64_t)%{n:a Ox.Addr.Ext.t}); |} : Int64.t)]
         : Int64.Hex.t)]
    ;;

    module Imm = struct
      include Imm

      let sexp_of_t (type a : any) (_ : a -> Sexp.t) n =
        [%sexp
          ([%c.no_alloc ({| return ((uint64_t)%{n:a Ox.Addr.Ext.Imm.t}); |} : Int64.t)]
           : Int64.Hex.t)]
      ;;
    end
  end
end
