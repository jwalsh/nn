#!/bin/bash

# Create and populate simple-ffn.mmd
cat << 'EOF' > simple-ffn.mmd
graph LR
    I[Input Layer] --> H[Hidden Layer]
    H --> O[Output Layer]
    style I fill:#f9f,stroke:#333,stroke-width:4px
    style H fill:#bbf,stroke:#333,stroke-width:4px
    style O fill:#bfb,stroke:#333,stroke-width:4px
EOF

# Create and populate deep-ffn.mmd
cat << 'EOF' > deep-ffn.mmd
graph LR
    I[Input Layer] --> H1[Hidden Layer 1]
    H1 --> H2[Hidden Layer 2]
    H2 --> H3[Hidden Layer 3]
    H3 --> O[Output Layer]
    style I fill:#f9f,stroke:#333,stroke-width:4px
    style H1 fill:#bbf,stroke:#333,stroke-width:4px
    style H2 fill:#bbf,stroke:#333,stroke-width:4px
    style H3 fill:#bbf,stroke:#333,stroke-width:4px
    style O fill:#bfb,stroke:#333,stroke-width:4px
EOF

# Create and populate wide-network.mmd
cat << 'EOF' > wide-network.mmd
graph LR
    I[Input Layer] --> H[Wide Hidden Layer]
    H --> O[Output Layer]
    style I fill:#f9f,stroke:#333,stroke-width:4px
    style H fill:#bbf,stroke:#333,stroke-width:4px,width:200px
    style O fill:#bfb,stroke:#333,stroke-width:4px
EOF

# Create and populate bottleneck-network.mmd
cat << 'EOF' > bottleneck-network.mmd
graph LR
    I[Input Layer] --> H1[Hidden Layer 1]
    H1 --> B[Bottleneck Layer]
    B --> H2[Hidden Layer 2]
    H2 --> O[Output Layer]
    style I fill:#f9f,stroke:#333,stroke-width:4px
    style H1 fill:#bbf,stroke:#333,stroke-width:4px
    style B fill:#fbb,stroke:#333,stroke-width:4px
    style H2 fill:#bbf,stroke:#333,stroke-width:4px
    style O fill:#bfb,stroke:#333,stroke-width:4px
EOF

# Create and populate multi-branch-network.mmd
cat << 'EOF' > multi-branch-network.mmd
graph TB
    I[Input Layer] --> H1[Hidden Layer 1]
    I --> H2[Hidden Layer 2]
    H1 --> O[Output Layer]
    H2 --> O
    style I fill:#f9f,stroke:#333,stroke-width:4px
    style H1 fill:#bbf,stroke:#333,stroke-width:4px
    style H2 fill:#bbf,stroke:#333,stroke-width:4px
    style O fill:#bfb,stroke:#333,stroke-width:4px
EOF

# Create and populate digit-identification-nn.mmd
cat << 'EOF' > digit-identification-nn.mmd
graph LR
    subgraph Input
        I[Input Layer<br/>100 neurons<br/>Flattened 10x10 image]
    end
    subgraph Hidden_Layers
        H1[Hidden Layer 1<br/>80 neurons<br/>ReLU]
        H2[Hidden Layer 2<br/>10 neurons<br/>ReLU]
        H3[Hidden Layer 3<br/>8 neurons<br/>ReLU]
    end
    subgraph Output
        O[Output Layer<br/>3 neurons<br/>Softmax]
    end
    I --> H1
    H1 --> H2
    H2 --> H3
    H3 --> O
    
    style I fill:#f9f,stroke:#333,stroke-width:2px
    style H1 fill:#bbf,stroke:#333,stroke-width:2px
    style H2 fill:#bbf,stroke:#333,stroke-width:2px
    style H3 fill:#bbf,stroke:#333,stroke-width:2px
    style O fill:#bfb,stroke:#333,stroke-width:2px

    Input_Image[10x10 Digit Image] --> Flatten[Flatten to 100x1 vector]
    Flatten --> I
    O --> |Probability for each class| Class1[Class 1]
    O --> |Probability for each class| Class2[Class 2]
    O --> |Probability for each class| Class3[Class 3]

    style Input_Image fill:#ff9,stroke:#333,stroke-width:2px
    style Flatten fill:#ff9,stroke:#333,stroke-width:2px
    style Class1 fill:#9f9,stroke:#333,stroke-width:2px
    style Class2 fill:#9f9,stroke:#333,stroke-width:2px
    style Class3 fill:#9f9,stroke:#333,stroke-width:2px
EOF

# Create and populate mnist-nn-architecture.mmd
cat << 'EOF' > mnist-nn-architecture.mmd
graph TD
    I[Input Layer<br/>784 neurons<br/>28x28 flattened image] --> H1[Hidden Layer 1<br/>128 neurons<br/>ReLU]
    H1 --> H2[Hidden Layer 2<br/>64 neurons<br/>ReLU]
    H2 --> O[Output Layer<br/>10 neurons<br/>Softmax]
    
    style I fill:#f9f,stroke:#333,stroke-width:2px
    style H1 fill:#bbf,stroke:#333,stroke-width:2px
    style H2 fill:#bbf,stroke:#333,stroke-width:2px
    style O fill:#bfb,stroke:#333,stroke-width:2px
    
    Input[28x28 MNIST Image] --> Flatten[Flatten to 784x1 vector]
    Flatten --> I
    O -->|Probability| D0[Digit 0]
    O -->|Probability| D1[Digit 1]
    O -->|Probability| D2[Digit 2]
    O -->|Probability| D3[...]
    O -->|Probability| D9[Digit 9]
    
    style Input fill:#ff9,stroke:#333,stroke-width:2px
    style Flatten fill:#ff9,stroke:#333,stroke-width:2px
    style D0 fill:#9f9,stroke:#333,stroke-width:2px
    style D1 fill:#9f9,stroke:#333,stroke-width:2px
    style D2 fill:#9f9,stroke:#333,stroke-width:2px
    style D3 fill:#9f9,stroke:#333,stroke-width:2px
    style D9 fill:#9f9,stroke:#333,stroke-width:2px
EOF

echo "All Mermaid diagram files have been created successfully."
