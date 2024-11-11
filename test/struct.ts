import { Print, Mul, Add, Sigmoid, AssertEq } from "./std";

// Input -> Hidden layer weights
type W1 = [
    [0.15, 0.25], // weights for hidden neuron 1
    [0.20, 0.30]  // weights for hidden neuron 2
];

// Hidden -> Output layer weights
type W2 = [
    [0.40, 0.50], // weights for output neuron
];

// Biases for each layer
type B1 = [0.35, 0.35]; // hidden layer biases
type B2 = [0.60];       // output layer bias

// Compute one neuron's output given inputs and weights
type NeuronOutput<
    Inputs extends number[],
    Weights extends number[],
    Bias extends number
> = Sigmoid<
    Add<
        Bias,
        WeightedSum<Inputs, Weights>
    >
>;

// Helper to compute weighted sum (dot product)
type WeightedSum<
    Inputs extends number[],
    Weights extends number[],
    Sum extends number = 0
> = Inputs extends []
    ? Sum
    : Inputs extends [infer X extends number, ...infer RestI extends number[]]
      ? Weights extends [infer W extends number, ...infer RestW extends number[]]
        ? WeightedSum<RestI, RestW, Add<Sum, Mul<X, W>>>
        : never
      : never;

// Forward pass through the network
type ForwardPass<Input extends number[]> = {
    // Hidden layer
    hidden: [
        NeuronOutput<Input, W1[0], B1[0]>,
        NeuronOutput<Input, W1[1], B1[1]>
    ],
    // Output layer
    output: [
        NeuronOutput<
            [
                NeuronOutput<Input, W1[0], B1[0]>,
                NeuronOutput<Input, W1[1], B1[1]>
            ],
            W2[0],
            B2[0]
        >
    ]
};

// Test the network with sample input
type Input = [0.05, 0.10];

type Main<Args extends string[]> = Print<ForwardPass<Input>>;