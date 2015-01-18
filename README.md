# relsandbox

A basic Erlang OTP app used for testing release and appup tooling.

## Build and Run

    $ rebar3 compile
    $ ./start.sh

## Versions and Upgrades

Using the following git tags, move between versions to test upgrades
etc.

### v1

Basic app with supervisor, one gen_server that counts with get/inc api.

### v2 [v1...v2](https://github.com/RJ/relsandbox/compare/v1...v2)

Add a decrement convenience function, no ````#state{}```` change.

#### Expected .appup

    {"2",
        [{"1", [
            {load_module, relsandbox_counter}
        ]}],
        [{"1", [
            {load_module, relsandbox_counter}
        ]}]
    }.

### v3 [v2...v3](https://github.com/RJ/relsandbox/compare/v2...v3)

Add name field to ````#state{}```` and get/set API.
This requires a ````code_change```` function to upgrade the state
record.

#### Expected .appup

    {"3",
        [{"2", [
            {update, relsandbox_counter, {advanced, []}}
        ]}],
        [{"2", [
            {update, relsandbox_counter, {advanced, []}}
        ]}]
    }.
