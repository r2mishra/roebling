
- Main Module
    - Parse the options
    ```
	rate               int
	duration           time.Duration
	timeout            time.Duration
	method             string
	headers            []string
	body               string
	bodyFile           string
	maxBody            int64
	workers            uint64
	maxWorkers         uint64
	connections        int
	noHTTP2            bool
	localAddress       string
	noKeepAlive        bool
	buckets            string
	resolvers          string
	insecureSkipVerify bool
	tlsCertFile        string
	tlsKeyFile         string
	caCert             string
    ```
    - Parse the target `string`

    - Instantiate an `attacker`

    - Instantiate a `GUI`
    
    - Run `GUI` with params

- GUI Module
    - Set up the widgets
    - Add Keybinds
    - For `Keyboard.ENTER`, run attacker
    - Redraw on every interval

- Attacker Module
    - Instantiate a Pacer that sets up the rate of attack (decides if another request is to be made)
    - Instantiate a Metrics module that stores the result of each attack
    - Instantiate a Storage module that saves the metric on which the calculation can be done.

- Metrics Module
    - Load the metrics store
    - Compute the metrics & charts
    - Invoked by the GUI module every interval.