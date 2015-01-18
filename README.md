# relsandbox

A basic Erlang OTP app used for testing release and appup tooling.


## Build and Run

    $ rebar3 compile
    $ ./start.sh

## Versions and Upgrades

Using the following git tags, move between versions to test upgrades
etc.

NB: I don't care about downgrades, and probably won't be testing them.

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

### v4 [v3...v4](https://github.com/RJ/relsandbox/compare/v3...v4)

Add a new module, ````relsandbox_phonebook```` with get/set API.
This requires a new child to be started under ````relsandbox_sup````

Also add a boring utility module, ````relsandbox_boring````.

#### Expected .appup

    {"4",
        [{"3", [
            {load_module, relsandbox_boring},
            {load_module, relsandbox_phonebook},
            {update, relsandbox_sup, supervisor},
            {apply, {supervisor, restart_child, [relsandbox_sup, relsandbox_phonebook]}}
        ]}],
        [{"3", [
            {apply, {supervisor, terminate_child, [relsandbox_sup, relsandbox_phonebook]}},
            {apply, {supervisor, delete_child,    [relsandbox_sup, relsandbox_phonebook]}},
            {update, relsandbox_sup, supervisor},
            {delete_module, relsandbox_phonebook},
            {delete_module, relsandbox_boring}
        ]}]
    }.

### v5 [v4...v5](https://github.com/RJ/relsandbox/compare/v4...v5)

Remove module, ````relsandbox_boring````.
Add dependency on lager (no appup instructions needed for this).

#### Expected .appup

    {"5",
        [{"4", [
            {delete_module, relsandbox_boring}
        ]}],
        [{"4", [
            {load_module, relsandbox_boring}
        ]}]
    }.
