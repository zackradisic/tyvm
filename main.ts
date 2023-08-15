// import { Print, Add, Sub, Lte, Eq } from "std";
import { Print, Add, Lte, Eq } from "./std";

type Lmao = 69;

type IsFunnyNum<V extends number> = Add<34, 35> extends V ? true : false;

type Main = Print<IsFunnyNum<"HI">>;
