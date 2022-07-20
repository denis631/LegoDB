module type Marshaller = sig
  type t
  type v

  val marshal : t -> v
  val unmarshal : v -> t
end

