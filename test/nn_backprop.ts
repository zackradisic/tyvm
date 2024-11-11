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

// Network parameters
type W1 = [
    [0.15, 0.25], // weights for first neuron
    [0.20, 0.30]  // weights for second neuron
];
type B1 = [0.35, 0.35];
type W2 = [0.40, 0.50];
type B2 = 0.60;

// Helper for sigmoid derivative
type SigmoidDerivative<X extends number> = Mul<
    Sigmoid<X>,
    Sub<1, Sigmoid<X>>
>;

// Compute neuron's pre-activation (z) and activation (a)
type NeuronCompute<
    Inputs extends number[],
    Weights extends number[],
    Bias extends number
> = {
    z: Add<
        Add<
            Bias,
            Mul<Inputs[0], Weights[0]>
        >,
        Mul<Inputs[1], Weights[1]>
    >,
    a: Sigmoid<
        Add<
            Add<
                Bias,
                Mul<Inputs[0], Weights[0]>
            >,
            Mul<Inputs[1], Weights[1]>
        >
    >
};

// Forward pass
type Forward<Input extends number[]> = {
    hidden: [
        NeuronCompute<Input, W1[0], B1[0]>,
        NeuronCompute<Input, W1[1], B1[1]>
    ],
    output: NeuronCompute<
        [
            NeuronCompute<Input, W1[0], B1[0]>["a"],
            NeuronCompute<Input, W1[1], B1[1]>["a"]
        ],
        W2,
        B2
    >
};

// Compute gradients
type Backward<
    Input extends number[],
    Target extends number,
    FwdPass extends Forward<Input>
> = {
    // Output layer error
    outputError: Sub<FwdPass["output"]["a"], Target>,
    
    // Output layer gradients
    dW2: [
        Mul<
            Mul<
                Sub<FwdPass["output"]["a"], Target>,
                SigmoidDerivative<FwdPass["output"]["z"]>
            >,
            FwdPass["hidden"][0]["a"]
        >,
        Mul<
            Mul<
                Sub<FwdPass["output"]["a"], Target>,
                SigmoidDerivative<FwdPass["output"]["z"]>
            >,
            FwdPass["hidden"][1]["a"]
        >
    ],
    dB2: Mul<
        Sub<FwdPass["output"]["a"], Target>,
        SigmoidDerivative<FwdPass["output"]["z"]>
    >,

    // Hidden layer gradients
    dW1: [
        [
            Mul<
                Mul<
                    Mul<
                        Sub<FwdPass["output"]["a"], Target>,
                        SigmoidDerivative<FwdPass["output"]["z"]>
                    >,
                    W2[0]
                >,
                Mul<
                    SigmoidDerivative<FwdPass["hidden"][0]["z"]>,
                    Input[0]
                >
            >,
            Mul<
                Mul<
                    Mul<
                        Sub<FwdPass["output"]["a"], Target>,
                        SigmoidDerivative<FwdPass["output"]["z"]>
                    >,
                    W2[0]
                >,
                Mul<
                    SigmoidDerivative<FwdPass["hidden"][0]["z"]>,
                    Input[1]
                >
            >
        ],
        [
            Mul<
                Mul<
                    Mul<
                        Sub<FwdPass["output"]["a"], Target>,
                        SigmoidDerivative<FwdPass["output"]["z"]>
                    >,
                    W2[1]
                >,
                Mul<
                    SigmoidDerivative<FwdPass["hidden"][1]["z"]>,
                    Input[0]
                >
            >,
            Mul<
                Mul<
                    Mul<
                        Sub<FwdPass["output"]["a"], Target>,
                        SigmoidDerivative<FwdPass["output"]["z"]>
                    >,
                    W2[1]
                >,
                Mul<
                    SigmoidDerivative<FwdPass["hidden"][1]["z"]>,
                    Input[1]
                >
            >
        ]
    ],
    dB1: [
        Mul<
            Mul<
                Sub<FwdPass["output"]["a"], Target>,
                SigmoidDerivative<FwdPass["output"]["z"]>
            >,
            Mul<
                W2[0],
                SigmoidDerivative<FwdPass["hidden"][0]["z"]>
            >
        >,
        Mul<
            Mul<
                Sub<FwdPass["output"]["a"], Target>,
                SigmoidDerivative<FwdPass["output"]["z"]>
            >,
            Mul<
                W2[1],
                SigmoidDerivative<FwdPass["hidden"][1]["z"]>
            >
        >
    ]
};

type LearningRate = 0.1;

type Input = [0.05, 0.10];
type Target = 0.01;

// Run one training step and return updated parameters
type Step = {
    fwd: Forward<Input>,
    grad: Backward<Input, Target, Forward<Input>>,
    newParams: {
        W1: [
            [
                Sub<W1[0][0], Mul<LearningRate, Backward<Input, Target, Forward<Input>>["dW1"][0][0]>>,
                Sub<W1[0][1], Mul<LearningRate, Backward<Input, Target, Forward<Input>>["dW1"][0][1]>>
            ],
            [
                Sub<W1[1][0], Mul<LearningRate, Backward<Input, Target, Forward<Input>>["dW1"][1][0]>>,
                Sub<W1[1][1], Mul<LearningRate, Backward<Input, Target, Forward<Input>>["dW1"][1][1]>>
            ]
        ],
        W2: [
            Sub<W2[0], Mul<LearningRate, Backward<Input, Target, Forward<Input>>["dW2"][0]>>,
            Sub<W2[1], Mul<LearningRate, Backward<Input, Target, Forward<Input>>["dW2"][1]>>
        ],
        B1: [
            Sub<B1[0], Mul<LearningRate, Backward<Input, Target, Forward<Input>>["dB1"][0]>>,
            Sub<B1[1], Mul<LearningRate, Backward<Input, Target, Forward<Input>>["dB1"][1]>>
        ],
        B2: Sub<B2, Mul<LearningRate, Backward<Input, Target, Forward<Input>>["dB2"]>>
    }
};

type Main<Args extends string[]> = Print<Step>;