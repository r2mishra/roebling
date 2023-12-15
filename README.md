# roebling

A command-line based load testing tool in Haskell that can plot metrics related to latency, request statuses, etc in real time. A minimalistic Haskell implementation of [ali](https://github.com/nakabonne/ali).

# Group Members
- Sumanth R Hegde
- Rohan Mishra
- Raj Nawal
- Om Prakaash

# Features
- Latency Metrics
- Pace requests at a user-specified rate
- Load Testing/Sudden spikes
- Simulate multiple users

# Design
- Parse command-line options
- Generate concurrent requests at given pace with a Pacer
- Plot latencies in real-time with a ASCII line chart using `brick`.
- Calculate aggregate metrics for bytes sent and received, percentiles for latency distribution, statistics for request success, etc

# Architecture
We have a basic setup to attack a given target endpoint at a given rate for a set number of times with the help of a pacer. We've replicated command-line arguments for [ali](https://github.com/nakabonne/ali), but haven't implemented all the features yet. There are two key components, each to be run in it's own thread: 
1. Attacker: An [`attacker`](./src/Lib.hs) function that sends a set number of requests to the target endpoint
2. UI: We're using brick widgets to replicate the GUI widgets in ali. The core plotting logic for the ASCII line chart has been borrowed from [madnight's asciichart](https://github.com/madnight/asciichart). The attacker dumps server responses into a channel. The GUI listens for updates on it's own channel and updates plots, metrics, etc via Brick's event-handling. We have a simple adapter which connects both the channels together.

The final UI looks as follows:



## Challenges
One of the challenges we faced were to setup the the async logic for pacing requests and communicating between the attacker and the GUI components. Working with an ASCII line chart is also a bit messy. We've had to make a hacky custom widget which monitors the current width of the plot and drops some of the earlier entries from the latency list in order to fit the plot in the terminal width.

# References
[Ali](https://github.com/nakabonne/ali) - Reference load testing library implemented in Go
[Vegeta](https://github.com/tsenart/vegeta) - Go library implementing Pacer and Attacker modules used in Ali 
[Asciichart](https://github.com/madnight/asciichart) - Reference for the core plotting logic.


# Setup instructions

Head over to [SETUP.md](/SETUP.md) for instructions on build management and running the server.
