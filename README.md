# Weave

Weave installs flow rules in OpenFlow switches in order to create a
flow between two entities in the network.  To use it, run the `weave`
tool.

Weave needs access to a Dobby database.  There are two ways of
providing this:

* specify the Erlang node name of a running Dobby instance,
  e.g. `dobby@127.0.0.1`

* provide a JSON file that Weave imports into an internal Dobby
  instance.  The contents of this Dobby instance is kept in the Mnesia
  directory.  To clear the local database, remove this directory.

Weave also needs connections to the relevant OpenFlow switches.  If no
switches are specified on the command line, Weave will listen on port
6653 and wait for the switches to connect to it.  Alternatively, you
can specify which switches Weave should connect to by giving the
hostname or IP address of each switch as command line arguments.
Those can optionally be followed by a colon and a port number, to use
a nonstandard port, e.g. `192.168.1.1:16653`.

Weave can perform two different actions: *create a net flow* between two
entities, or *install a tap rule*.  The former is the default.  The
latter is invoked by specifying `-tap` as the first argument.

## Create flow between entities

```
./weave [-demo] JSON-FILE-OR-NODE SOURCE DESTINATION [SWITCH ...]
```

Create a bidirectional flow between `SOURCE` and `DESTINATION`.
`SOURCE` and `DESTINATION` are the Dobby identifiers of the two
endpoints.

Weave finds a path in the graph in Dobby, through OpenFlow switches
and ports, and installs flow rules such that IP and ARP packets can be
sent between the endpoints.  Weave takes care that existing flow rules
are amended, not overwritten, as long as they have been recorded in
Dobby.

If Dobby contains the IP addresses of the endpoints (specified by `ip`
and optionally `netmask` metadata entries), then Weave will create
rules that match on those IP addresses.  Otherwise, Weave will create
rules that only match on the port packets arrive on.  This leads to
hub-like behaviour: packets are forwarded to every port that could
potentially be their intended destination.  This behaviour can be
explicitly requested by setting the `use_bridge_rules` metadata entry
to `true` on the `SOURCE` node.

Normally, Weave will both install the flow rules in the switches and
publish them to Dobby.  If the `-demo` flag is specified, it will only
publish flow rules to Dobby but not install them in the switches.

As described in the introduction, `JSON-FILE-OR-NODE` is either a JSON
file or the node name of a running Dobby node, and one or more switch
addresses may be specified at the end of the command line.

For example, to create a flow from `foo` to `bar` using a Dobby node
running on the same host, waiting for switches to connect:

```
./weave dobby@127.0.0.1 foo bar
```

To read network information from a JSON file, and connect to two
switches specified by IP addresses:

```
./weave info.json foo bar 10.0.0.1 10.0.0.2
```

## Install tap rule

```
./weave -tap JSON-FILE-OR-NODE SOURCE DESTINATION [SWITCH ...]
```

Find an existing net flow between `SOURCE` and `DESTINATION`, and
amend the flow rules such that they copy packets to the controller.
This is unidirectional: it applies to packets sent from `SOURCE` to
`DESTINATION`, not to packets sent from `DESTINATION` to `SOURCE`.

The tap rule is added at _each_ hop in the flow, so if there are three
switches between `SOURCE` and `DESTINATION`, the controller will get
three copies of each packet.

For example, to tap packets going from `foo` to `bar`, using a Dobby
node running on the same host, and connecting to a specific switch:

```
./weave -tap dobby@127.0.0.1 foo bar 10.0.0.1
```
