-module(chatterlang).

-export([login/3,login/4,post_link/4,post_status/3]).

login (Username, Password, SecurityToken)->
  sfdc:login(Username, Password, SecurityToken).
  

login (Username, Password, SecurityToken, Endpoint)->
    sfdc:login(Username, Password, SecurityToken, Endpoint).
    

post_link(Url, Description, SessionId, Endpoint)->
    ok.

post_status(Status, SessionId, Endpoint)->
    UserInfo=sfdc:get_user_info(SessionId, Endpoint),
    {_,_,Id}=lists:keyfind("userId", 1, UserInfo),
    UpdateStatusSobject=[
			 {"type", "string", "User"},
			 {"Id", "string", Id},
			 {"CurrentStatus", "string", Status}
			],
    
    sfdc:update(UpdateStatusSobject, SessionId, Endpoint).

    

