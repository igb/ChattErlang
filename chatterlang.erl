-module(chatterlang).

-export([login/3,login/4,post_link/4,update_status/3,post_file_to_group/7,post_file/5]).

login (Username, Password, SecurityToken)->
  sfdc:login(Username, Password, SecurityToken).
  

login (Username, Password, SecurityToken, Endpoint)->
    sfdc:login(Username, Password, SecurityToken, Endpoint).
    

post_link(Url, Description, SessionId, Endpoint)->
    Id=get_current_user_id(SessionId, Endpoint),
    PostLinkSobject=[
		     {"type", "string", "FeedPost"},
		     {"ParentId", "string", Id},
		     {"LinkUrl", "string", Url}
		    ],
    case Description of 
	"" -> sfdc:create(PostLinkSobject, SessionId, Endpoint);
	_ ->  sfdc:create(lists:flatten([PostLinkSobject,{"Title", "string", Description}]), SessionId, Endpoint)
    end.



update_status(Status, SessionId, Endpoint)->
    Id=get_current_user_id(SessionId, Endpoint),
    UpdateStatusSobject=[
			 {"type", "string", "User"},
			 {"Id", "string", Id},
			 {"CurrentStatus", "string", Status}
			],
    
    sfdc:update(UpdateStatusSobject, SessionId, Endpoint).



post_file(PostBody, Description, Name, File, SessionId, Endpoint)->
    Id=get_current_user_id(SessionId, Endpoint),
    PostFileObject=[
		    {"type", "string", "FeedItem"},
		    {"ParentId", "string", UserId},
		    {"Body", "string", PostBody},
		    {"ContentDescription", "string", Description},
		    {"ContentFileName", "string", Name},
		    {"ContentData", "base64Binary", base64:encode_to_string(File)}
		   ],
    sfdc:create(PostFileObject, SessionId, Endpoint).

											     

    
get_current_user_id(SessionId, Endpoint)->
    UserInfo=sfdc:get_user_info(SessionId, Endpoint),
    {_,_,Id}=lists:keyfind("userId", 1, UserInfo),
    Id.

