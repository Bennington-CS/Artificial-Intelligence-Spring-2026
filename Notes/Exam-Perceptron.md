# Prompt
For one (1) of the following concepts, please provide: (i) a concise and accurate definition; (ii) an explanation of the concept; (iii) an example of the concept; (iv) the contrast concept; (v) a definition of the contrast concept.
# Concept
Perceptron
# Sample Answer
A perceptron is a single artificial neuron.  It takes one or more inputs, multiplies each input by a weight, adds the weighted inputs together with a bias, and then passes the result through an activation function to produce an output.  By analogy, inputs are like signals coming in through dendrites; weights are like the strength of synaptic connections;  and the output is like the signal sent onward through an axon.

Suppose we have two inputs.  In general, inputs are initially scaled (0-1) so that the values are in a more easily trainable range.  Then each input (x1, x2) is multiplied by a weight (w1, w2) according to the summation formula: x1w1 + x2w2 + b.  The weights determine how strongly each input affects the final result.  A high positive weight means that an input strongly pushes the output upward.  A middling negative weight means that an input pushes the output moderately downward.  After the inputs are multiplied by their weights, the bias is added (directly adding its value), and an activation function is applied.  In a traditional perceptron, this is a Heaviside step function; in our book, it's a sigmoid.

You train a perceptron by repeatedly running through training data for which you already know the correct classifications. For each item, the perceptron takes the inputs, multiplies them by the current weights, adds the bias, and produces an output.  If the perceptron is wrong, the weights are adjusted so that the next output will be closer to the correct one (influenced by the learning rate).  Inputs that should have mattered more are given higher weights; inputs that pushed the answer in the wrong direction are given lower weights. Over many examples, this process gradually changes the weights and bias so that if the items are linearly separable, the perceptron will arrive at the correct decision boundary.

Consider, for example, whether an apartment will be rented within a month.  The inputs might be price and size.  Price might have a negative weight, because more expensive apartments are less likely to be rented.  Size might have a positive weight, because larger apartments may be more attractive.  The perceptron combines those two inputs and produces an output, such as a predicted probability that the apartment will be rented (if we suppose a sigmoid activation).

A perceptron is primarily applicable to linear classification problems; that is,cases where the items can be separated by a straight line (sometimes called the decision boundary), or by a plane or hyperplane (the higher-dimensional equivalent of a straight line). In the apartment example, if rented and not-rented apartments can be separated by a line based on price and size, then a perceptron can learn that boundary.

A contrast concept is a multi-node artificial neural network, which is an artificial neural network with hidden layers. A single perceptron has only one basic unit doing all the work.  An artificial neural network uses many perceptron-like elements together.  A neural network with multiple nodes and layers can model non-linear relationships.

