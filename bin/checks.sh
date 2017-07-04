#!/usr/bin/env sh

rebar3 as test do elvis && rebar3 as cowboy1 do xref, dialyzer, ct && rebar3 as cowboy2 do xref, dialyzer, ct
