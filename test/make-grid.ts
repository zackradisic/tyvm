import { Add, Lt, Print, SetArray } from "./std";

type gridSize = 28;

type MakeDefaultGrid<Row extends number, Col extends number> = NewArray<
  NewArray<0, gridSize>,
  gridSize
>;

type Main<Args extends string[]> = Print<MakeDefaultGrid<0, 0>>;
