%% -*- erlang -*-
%%! -smp enable -sname factorial -mnesia debug verbose
main([]) ->
  eqc_install:install().
