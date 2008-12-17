%% Types:
%%   id() = string()
%%   ip_address() = see inet module documentation



%%% These are frames that are sent over the broadcast radio or over the
%% network. Messages are included inside a frame with a header that gives
%% routing information.

%% From = origin | {ip, ip_address()} | {id, id()}
%% To = any | id()
%% Hops = integer() | infinity
%% Data = term()
-define(HEADER(From, To, Hops), {clusterl_header, From, To, Hops}).
-define(FRAME(Header, Data), {clusterl_frame, Header, Data}).

%% Id = id()
-define(ANNOUNCE(Id, Port), {announce, Id, Port}).
