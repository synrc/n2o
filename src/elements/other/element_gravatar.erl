% vim: sw=4 ts=4 et ft=erlang
-module (element_gravatar).
-compile(export_all).
-include_lib ("wf.hrl").

reflect() -> record_info(fields, gravatar).

render_element(Record) -> 
    Image = #image {
        html_id=Record#gravatar.html_id,
        id=Record#gravatar.id,
        anchor=Record#gravatar.anchor,
        image = gravatar_icon(Record)
    },
    element_image:render_element(Image).

gravatar_icon(G = #gravatar{size=Size}) when is_integer(Size) ->
	gravatar_icon(G#gravatar{size=wf:to_list(Size)});
gravatar_icon(#gravatar{email=Email, size=Size, rating=Rating, default=Default}) ->
    GravatarId = digest2str(erlang:md5(wf:clean_lower(Email))),
    wf:f("http://www.gravatar.com/avatar/~s?size=~s&r=~s&d=~s" ,
        [GravatarId, Size, Rating, Default]).

digest2str(Digest) ->
    [[nibble2hex(X bsr 4), nibble2hex(X band 15)] ||
    X <- binary_to_list(Digest)].

-define(IN(X,Min,Max), X >= Min, X =< Max).
nibble2hex(X) when ?IN(X, 0, 9)   -> X + $0;
nibble2hex(X) when ?IN(X, 10, 15) -> X - 10 + $a.
