import { Print, Add, Sub, Lte, Eq } from "./std";

type A<X extends number> = B<X>;
type B<X extends number> = A<X>;

type Main = Print<A<420>>;
