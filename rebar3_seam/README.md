# rebar3_seam

rebar3 plugin for [seam](https://github.com/emagkro/seam) condition-level coverage analysis.

Add the plugin to your project, run `rebar3 seam`, get condition coverage. One command replaces the manual start/instrument/test/analyse/stop workflow.

## Quickstart

Add to your `rebar.config`:

```erlang
{project_plugins, [rebar3_seam]}.
```

Run:

```
$ rebar3 seam
```

The plugin compiles your project (test profile), instruments every non-test module via seam, runs eunit, prints a coverage summary, and writes per-module reports to `_build/test/seam/`.

## Configuration

All options go in `rebar.config` under the `seam` key. CLI flags override config values.

```erlang
{seam, [
    {level, condition},       %% condition | decision (default: condition)
    {mode, full},             %% full | fast (default: full)
    {verbose, true},          %% print per-module table (default: false)
    {min_coverage, 80},       %% fail below this % (default: 0 = off)
    {output, html},           %% html | text (default: html)
    {excl_mods, [generated]}  %% modules to skip (default: [])
]}.
```

## CLI options

| Flag | Short | Description |
|------|-------|-------------|
| `--verbose` | `-v` | Print per-module coverage table |
| `--min_coverage N` | `-m` | Fail if total coverage < N% |
| `--reset` | `-r` | Zero counters before run |
| `--import PATH` | `-i` | Merge prior `seam.dat` before testing |
| `--suite MODS` | `-s` | Comma-separated module list to test |
| `--level LEVEL` | `-l` | `condition` or `decision` |
| `--mode MODE` | `-M` | `full` or `fast` |
| `--output FMT` | `-o` | `html` or `text` |

## Output

Reports land in `_build/test/seam/`. Coverage data exports to `_build/test/seam/seam.dat` for later import.

Summary always prints; `--verbose` adds the per-module breakdown:

```
  Seam condition coverage: 75.0% (6/8)

  Module                         Covered    Total        %
  ----------------------------------------------------------
  my_module                            4        5    80.0%
  my_other                             2        3    66.7%
  ----------------------------------------------------------
  TOTAL                                6        8    75.0%
```

## Requirements

OTP 27+. Target modules must be compiled with `debug_info`.
