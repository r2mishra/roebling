# roebling

Implement a command-line based load testing tool in Haskell that can plot metrics related to latency, request statuses, etc in real time. Our idea is to replicate some of the features of [ali](https://github.com/nakabonne/ali).

# Features
- Latency Metrics
- Load Testing/Sudden spikes
- Simulate multiple users
- Assess Max load for a given latency target

# Preliminary Design
Our first steps are to break down the project into the following tasks:
- Parsing command-line options
- Generating concurrent requests
- Storage Response results
- Calculate aggregate metrics
- Render UI using `brick` to generate real time plots and metrics.

# References
[Ali](https://github.com/nakabonne/ali) - Reference load testing library implemented in Go
[Vegeta](https://github.com/tsenart/vegeta) - Go library implementing Pacer and Attacker modules used in Ali 


