%%%-------------------------------------------------------------------
%%% Created : 30 Oct 2007 by Biorn X Oqvist <zrajm@kreditor.se>
%%% Descr.  : Yatsy code coverage module.
%%%-------------------------------------------------------------------
-module(yatsy_cover).
%%-compile(export_all).
-export([cover_compile_beams/1
         ,cover_compile_beams/2
         ,analyse_to_file/1
         ,get_beam_files/1
        ]).


%%%
%%% Return list of .beam files to cover-compile.
%%% Make it possible to customize this by calling a callback module.
%%%
get_beams(Path, Mod) ->
    Mod:get_beam_files(Path).

%%% FIXME the below should really be:
%%%    Prefix = filename:join([Path, "lib", "*", "ebin"]),
%%%    L = filelib:wildcard(filename:join([Prefix, "*.beam"])),
%%%    [File || File <- L,
%%%             false == lists:suffix("_SUITE.beam", File)].

get_beam_files(Path) ->
    Prefix = filename:join([Path, "lib", "*", "ebin"]),
    L = filelib:wildcard(filename:join([Prefix, "*.beam"])),
    [File || File <- L,
             false == lists:suffix("_SUITE.beam", File),
             false == lists:suffix("_yaws1.beam", File),
             false == lists:suffix("disk_log.beam", File),
             false == lists:suffix("iso639.beam", File)].


%%% cover compile all beams in our source dir
%%% (beams must have been compiled with "debug_info" set)
cover_compile_beams(Path) ->
    cover_compile_beams(Path, ?MODULE).

cover_compile_beams(Path, Mod) ->
  io:fwrite("Cover compiling .beam files in ~p:~n", [Path]),
  do_cover_compile_beams(get_beams(Path, Mod), []).

do_cover_compile_beams([Beam|RemainingBeams], Acc) ->
  io:fwrite("  ~p: ~p ", [
			  length(RemainingBeams),
			  list_to_atom(filename:basename(Beam, ".beam"))
			 ]),
  case cover:compile_beam(Beam) of
    {ok, BeamFile} ->
      io:fwrite("(ok)~n"),
      Acc2 = [BeamFile|Acc];
    {error, Reason} ->
      io:fwrite("(error; ~p)~n", [Reason]),
      Acc2 = Acc
  end,
  do_cover_compile_beams(RemainingBeams, Acc2);
%%
do_cover_compile_beams([], Acc) ->
  {ok, Acc}.



%% dump output for all cover analysed files
analyse_to_file(OutDir) ->
  %% FIXME: should any existing old OutDir be deleted here?
  file:make_dir(OutDir),
  io:fwrite("Putting cover analysis output in ~p:~n", [OutDir]),
  %io:fwrite("Writing output of cover analysis:~n"),
  analyse_to_file(OutDir, cover:modules(), []).

analyse_to_file(OutDir, [Module|RemainingModules], Acc) ->
  OutputFile = filename:join([
			      OutDir,
			      atom_to_list(Module)++".COVER.txt"
			     ]),
  io:fwrite("  ~p: ~p ", [length(RemainingModules), Module]),
  %io:fwrite("  ~p: ~p -> ~p ", [length(RemainingModules), Module, OutputFile]),
  case cover:analyse_to_file(Module, OutputFile) of
    {ok, OutFile} ->
      io:fwrite("(ok)~n"),
      Acc2 = [OutFile|Acc] ;
    {error, Reason} ->
      io:fwrite("(error; ~p)~n", [Reason]),
      Acc2 = Acc
  end,
  analyse_to_file(OutDir, RemainingModules, Acc2);

analyse_to_file(_, [], Acc) ->
  {ok, Acc}.

%%[[eof]]
