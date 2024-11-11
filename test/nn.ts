export type Print<T> = T;
export type Rand<Min extends number, Max extends number> = number;
export type Mod<A extends number, B extends number> = number;
export type Add<A extends number, B extends number> = number;
export type Sub<A extends number, B extends number> = number;
export type Mul<A extends number, B extends number> = number;
export type Div<A extends number, B extends number> = number;
export type Exp<A extends number, B extends number> = number;
export type Lte<A extends number, B extends number> = boolean;
export type Lt<A extends number, B extends number> = boolean;
export type Gte<A extends number, B extends number> = boolean;
export type Eq<A extends number, B extends number> = boolean;
export type And<A extends boolean, B extends boolean> = boolean;
export type Or<A extends boolean, B extends boolean> = boolean;

type E = 2.718281828459045;

type Sigmoid<A extends number> = Div<
    1,
    Add<1, Exp<E, Mul<A, -1>>>
>;

// Two neurons in hidden layer
type W1 = [
    [0.15, 0.25], // weights for first neuron
    [0.20, 0.30]  // weights for second neuron
];
type B1 = [0.35, 0.35];

// Output layer weights and bias
type W2 = [0.40, 0.50]; // weights for output neuron
type B2 = 0.60;

// Compute one neuron's output
type NeuronOutput<
    Inputs extends number[],
    Weights extends number[],
    Bias extends number
> = Sigmoid<
    Add<
        Add<
            Bias,
            Mul<Inputs[0], Weights[0]>
        >,
        Mul<Inputs[1], Weights[1]>
    >
>;

type Input = [0.05, 0.10];

// Compute both hidden neurons in parallel
type HiddenLayer = [
    NeuronOutput<Input, W1[0], B1[0]>,
    NeuronOutput<Input, W1[1], B1[1]>
];

// Compute final output
type Output = NeuronOutput<HiddenLayer, W2, B2>;

type Main<Args extends string[]> = Print<Output>;