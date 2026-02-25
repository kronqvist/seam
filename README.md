# Seam

Condition-level coverage analysis for Erlang.

## Premise

OTP's `cover` tracks which lines execute and how often. That is the wrong
granularity for test generation. A guard like `X > 10, Y < 5` is one line but
two conditions — cover cannot tell you which condition your tests never falsified.
Mutation testers, fuzzers, and property-based test generators need denser
feedback: which individual Boolean conditions have been exercised both ways, and
which remain stuck on one side.

Smother (Ramsay, 2014) solved this for Erlang R16 with MC/DC instrumentation.
It is unmaintained. Seam is a modern reimplementation of the core idea — BEAM
bytecode instrumentation at the condition level — optimized for test generation
feedback rather than certification compliance.

## How it works

Seam extracts the abstract code from a compiled BEAM file, rewrites function
clauses to record each guard condition's true/false outcome at runtime, and
recompiles. Guards are left unmodified (Erlang restricts guard expressions to
BIFs); tracking calls execute in the clause body, with prior clauses'
conditions re-evaluated via try/catch to capture failure data. The result is a
drop-in replacement: instrumented modules behave identically to originals.

## Example

```erlang
%% Start the coverage server, instrument a module
seam:start().
seam:compile_beam(my_module).

%% Exercise the code
my_module:classify(15),
my_module:classify(5),
my_module:classify(-1),

%% Query condition coverage
seam:condition_coverage(my_module).
%% => #{{my_module, classify, 1, 1} => {1, 2},   % X > 10: true 1x, false 2x
%%     {my_module, classify, 2, 1} => {1, 1}}     % X > 0:  true 1x, false 1x

%% Decision coverage
seam:decision_coverage(my_module).
%% => #{{my_module, classify, 1} => {1, 2},
%%     {my_module, classify, 2} => {1, 1},
%%     {my_module, classify, 3} => {1, 0}}

%% Source-annotated HTML report
seam_report:html_to_file(my_module, "coverage.html").

%% Restore originals, tear down
seam:stop().
```

## API at a glance

**Lifecycle:** `start/0`, `stop/0`, `reset/0`

**Instrumentation:** `compile_beam/1` — accepts a module atom or path to a
`.beam` file compiled with `debug_info`

**Coverage queries:** `condition_coverage/1`, `decision_coverage/1`,
`analyse/2`

**Reports:** `report/2` (text or html iolist), `seam_report:html_to_file/2`

**Persistence:** `export/1`, `import/1` — serialize coverage data to disk,
merge across runs

**Analysis:** `seam_analyse:condition_summary/1`,
`seam_analyse:decision_summary/1`, `seam_analyse:untested_conditions/1`,
`seam_analyse:mcdc_coverage/1`

Full API documentation: `rebar3 edoc`.

## Testing

```sh
rebar3 eunit
```

## Requirements

- OTP 27+
- `debug_info` on target modules (standard rebar3 default)
