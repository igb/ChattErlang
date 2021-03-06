 %% @author Ian Brown <igb@hccp.org>
 %%   [http://www.hccp.org]

-module(sfdc).
-export([login/3, login/4, update/3, get_user_info/2, get_user_info_sobject_from_soap_response/1, soql_query/3, soql_query_all/3, soql_query_more/3, get_all_results_for_query/3, create/3, delete/3, get_server_timestamp/2, logout/2, get_deleted/5, erlang_date_to_xsd_date_time/1,integer_pad/1,describe_sobject/3,describe_sobjects/3, describe_global/2, describe_data_category_groups/3,describe_tabs/2, describe_softphone_layout/2, describe_layout/3, describe_layout/4, upsert/4, merge/5, search/3, set_password/4, reset_password/3, send_email/3, convert_lead/11, process_submit/5, process_workitem/6, get_process_response/4, invalidate_sessions/3, empty_recycle_bin/3, undelete/3, retrieve/5, get_updated/5]).



-define(DEFAULT_ENDPOINT, "https://www.salesforce.com/services/Soap/u/18.0").


%% OPERATION: Login


login (Username, Password, SecurityToken)->
    login (Username, Password, SecurityToken, ?DEFAULT_ENDPOINT).

login (Username, Password, SecurityToken, Endpoint)->
    LoginXml=create_login_request(Username, lists:append([Password, SecurityToken])),
     {ok, {{_, ResponseCode, _}, _, ResponseBody}}=httpc:request(post, {Endpoint, [{"SOAPAction:", "\"\""}], "text/xml", LoginXml}, [],[]),
    case(ResponseCode) of
	200  -> io_lib:write_string(ResponseBody);
	500 -> io_lib:write_string(ResponseBody)
    end,
    SimplifiedXml=parse_xml(ResponseBody),
    case is_soap(SimplifiedXml) of 
	ok -> Body=get_body_from_envelope(SimplifiedXml),process_body_for_login(Body);
	_ -> {err, "Received response that was not not SOAP"}
    end.
 
	    
process_body_for_login (Body) ->	
    case is_fault(Body) of 
	ok -> {err, get_fault(Body)};
	_ -> extract_successful_login_info(Body)
    end.
	    
extract_successful_login_info(Body)->				   
    LoginResponse=get_body_content(Body),
    {_,_,LoginResponseChildren}=LoginResponse,
    [Result]=LoginResponseChildren,
    {_,_,ResultChildren}=Result,
    [_,_,_,ServerUrlElement,SessionIdElement|_]=ResultChildren,
    {_,_,[ServerUrl]}=ServerUrlElement,
    {_,_,[SessionId]}=SessionIdElement,
    [{sessionId,SessionId},{serverUrl,ServerUrl}].
    
    
create_login_request (Username, Password)->
    lists:append([create_xml_declaration(), create_soap_envelope([], create_soap_body(create_login_block(Username, Password)))]).

create_session_header(SessionId)->
    lists:append(["<SessionHeader xmlns=\"urn:partner.soap.sforce.com\"><sessionId>", SessionId, "</sessionId></SessionHeader>"]).


%% OPERATION: Update


update(SObject, SessionId, Endpoint)->
     UpdateBody=create_update(SObject),
    Result=send_sforce_soap_message(UpdateBody, SessionId, Endpoint),
    case Result of
	[{id,[],[Id]},{success,[],["true"]}]-> {ok, Id};
	_->Result
    end.
    


create_update(SObject)->
    lists:append(["<m:update xmlns:m=\"urn:partner.soap.sforce.com\" xmlns:sobj=\"urn:sobject.partner.soap.sforce.com\"><m:sObjects>", get_xml_for_sobject(SObject,[]), "</m:sObjects></m:update>"]).


% OPERATION: getUserInfo
get_user_info(SessionId, Endpoint)->
    SessionHeader=create_session_header(SessionId),
    GetUserInfoBody="<getUserInfo xmlns=\"urn:partner.soap.sforce.com\"/>",
    GetUserInfoSoapMessage=create_soap_envelope(create_soap_header(SessionHeader), create_soap_body(GetUserInfoBody)),
    SoapResponse=send_soap_message(GetUserInfoSoapMessage, Endpoint),
    get_user_info_sobject_from_soap_response(SoapResponse).

get_user_info_sobject_from_soap_response(SoapResponse)->
    Xml=parse_xml(SoapResponse),
    BodyXml=get_body_from_envelope(Xml),
    {getUserInfoResponse,_,[{result,_,UserInfoXml}]}=get_body_content(BodyXml),
    convert_xml_to_sobject(UserInfoXml,[]).



%OPERATION: Query

soql_query(QueryString, SessionId, Endpoint)->
    SessionHeader=create_session_header(SessionId),
    QueryBody=lists:append(["<query xmlns=\"urn:partner.soap.sforce.com\"><queryString>", QueryString, "</queryString></query>"]),
    QuerySoapMessage=create_soap_envelope(create_soap_header(SessionHeader), create_soap_body(QueryBody)),
    SoapResponse=send_soap_message(QuerySoapMessage, Endpoint),
    get_query_results_from_soap_response(SoapResponse).


soql_query_all(QueryString, SessionId, Endpoint)->
    SessionHeader=create_session_header(SessionId),
    QueryBody=lists:append(["<queryAll xmlns=\"urn:partner.soap.sforce.com\"><queryString>", QueryString, "</queryString></queryAll>"]),
    QuerySoapMessage=create_soap_envelope(create_soap_header(SessionHeader), create_soap_body(QueryBody)),
    SoapResponse=send_soap_message(QuerySoapMessage, Endpoint),
    get_query_results_from_soap_response(SoapResponse).

soql_query_more(QueryLocator, SessionId, Endpoint)->
    SessionHeader=create_session_header(SessionId),
    QueryBody=lists:append(["<queryMore xmlns=\"urn:partner.soap.sforce.com\"><queryLocator>", QueryLocator, "</queryLocator></queryMore>"]),
    QuerySoapMessage=create_soap_envelope(create_soap_header(SessionHeader), create_soap_body(QueryBody)),
    SoapResponse=send_soap_message(QuerySoapMessage, Endpoint),
    get_query_results_from_soap_response(SoapResponse).


get_all_results_for_query(QueryString, SessionId, Endpoint)->
    {IsDone, QueryLocator, _, Results}=soql_query(QueryString, SessionId, Endpoint),
    case IsDone of
	"true"->
	    Results;
	"false"->
	    lists:append(Results, get_more_results_for_query(QueryLocator, SessionId, Endpoint))
    end.

get_more_results_for_query(QueryLocator, SessionId, Endpoint)->
    {IsDone, QueryLocator, _, Results}=soql_query_more(QueryLocator, SessionId, Endpoint),
    case IsDone of
	"true"->
	    Results;
	"false"->
	    lists:append(Results, get_more_results_for_query(QueryLocator, SessionId, Endpoint))
    end.
									      

    

get_query_results_from_soap_response(SoapResponse)->
    Xml=parse_xml(SoapResponse),
    BodyXml=get_body_from_envelope(Xml),
    {_,_,[{result,_,QueryResponse}]}=get_body_content(BodyXml),
    [{done,_,[IsDone]}, {queryLocator,_,QueryLocator}|TheRest]=QueryResponse,
    {SizeInt, SobjectRecords}=get_query_results_from_record_set(TheRest),
    {IsDone, lists:flatten(QueryLocator), SizeInt, SobjectRecords}.


get_query_results_from_record_set(RecordSet)->
    get_query_results_from_record_set(RecordSet,[]).

get_query_results_from_record_set([H|T],Results)->
    
    case H of 
	{records,_,_}->
	    {records,[{'xsi:type',_}],Records}=H,
	    %io:fwrite("~s", Records),
	    RecordsSoFar=lists:append(Results, [convert_xml_to_sobject(Records, [])]),
	    get_query_results_from_record_set(T,RecordsSoFar);
	{size,_,_}->
	    {_,_,[Size]}=H,
	    {SizeInt,_}=string:to_integer(Size),
	    {SizeInt, Results}
    end;
get_query_results_from_record_set([],Results)->
    Results.
    



%OPERATION: Create


create(Sobject, SessionId, Endpoint)->
    SessionHeader=create_session_header(SessionId),
    CreateBody=  lists:append(["<m:create xmlns:m=\"urn:partner.soap.sforce.com\" xmlns:sobj=\"urn:sobject.partner.soap.sforce.com\"><m:sObjects>", get_xml_for_sobject(Sobject,[]), "</m:sObjects></m:create>"]),
    CreateSoapMessage=create_soap_envelope(create_soap_header(SessionHeader), create_soap_body(CreateBody)),
    SoapResponse=send_soap_message(CreateSoapMessage, Endpoint),
    get_create_results_from_soap_response(SoapResponse).
    
get_create_results_from_soap_response(SoapResponse)->
    Xml=parse_xml(SoapResponse),
    BodyXml=get_body_from_envelope(Xml),
    BodyContent=get_body_content(BodyXml),
    {_,_,[{result,_,CreateResponse}]}=BodyContent,
    case CreateResponse of
	[{id,[],[ObjectId]},{success,[],["true"]}]->{ok,ObjectId};
	[{errors,[],[_, {message,[], Message}|_]},{success,[],["false"]}]->{err,Message};
	_ ->{err, "unknown error has occurred"}
    end.



%OPERATION: Delete

delete(Id, SessionId, Endpoint)->
    SessionHeader=create_session_header(SessionId),
    DeleteBody=  lists:append(["<delete xmlns=\"urn:partner.soap.sforce.com\"><ids>", Id, "</ids></delete>"]),
    DeleteSoapMessage=create_soap_envelope(create_soap_header(SessionHeader), create_soap_body(DeleteBody)),
    SoapResponse=send_soap_message(DeleteSoapMessage, Endpoint),
    get_create_results_from_soap_response(SoapResponse).



%OPERATION: getDeleted

get_deleted(ObjectType, StartDate, EndDate, SessionId, Endpoint)->
    SessionHeader=create_session_header(SessionId),
    GetDeletedBody=lists:append(["<getDeleted xmlns=\"urn:partner.soap.sforce.com\"><sObjectType>", ObjectType,"</sObjectType><startDate>",erlang_date_to_xsd_date_time(StartDate),"</startDate><endDate>",erlang_date_to_xsd_date_time(EndDate),"</endDate></getDeleted>"]),
    GetDeletedSoapMessage=create_soap_envelope(create_soap_header(SessionHeader), create_soap_body(GetDeletedBody)),
    SoapResponse=send_soap_message(GetDeletedSoapMessage, Endpoint),
    get_deleted_results_from_soap_response(SoapResponse).

get_deleted_results_from_soap_response(SoapResponse)->
    Xml=parse_xml(SoapResponse),
    BodyXml=get_body_from_envelope(Xml),
    {_,_,[{result,_,DeletedResponse}]}=get_body_content(BodyXml),
    [{earliestDateAvailable,[],[EarliestDateAvailable]},
     {latestDateCovered,[],[LatestDateAvailable]}]=DeletedResponse,
    [{"earliestDateAvailable", xsd_datetime_to_erlang_datetime(EarliestDateAvailable)}, {"latestDateCovered", xsd_datetime_to_erlang_datetime(LatestDateAvailable)}].



%OPERATION: getServerTimeStamp

get_server_timestamp(SessionId, Endpoint)->
   
    ServerTimestampBody="<getServerTimestamp xmlns=\"urn:partner.soap.sforce.com\"/>",
    [{timestamp,[],[TimeStamp]}]=send_sforce_soap_message(ServerTimestampBody, SessionId, Endpoint),
    xsd_datetime_to_erlang_datetime(TimeStamp).


%OPERATION: describeSOBject
describe_sobject(Type,SessionId, Endpoint)-> 
    DescribeSobjectBody=lists:append(["<describeSObject xmlns=\"urn:partner.soap.sforce.com\">", "<sObjectType>", Type, "</sObjectType></describeSObject>"]),
    Response=send_sforce_soap_message(DescribeSobjectBody, SessionId, Endpoint),
    get_value_from_sobject_xml(Response).

%OPERATION: describeSOBjects
describe_sobjects(Types,SessionId, Endpoint)-> 
    DescribeSobjectBody=lists:append(["<describeSObjects xmlns=\"urn:partner.soap.sforce.com\">", get_describe_sobjects_message(Types,[]),"</describeSObjects>"]),
    Response=send_sforce_soap_message(DescribeSobjectBody, SessionId, Endpoint),
    case Response of 
	{err,_}->Response;
	_-> case length(Response) of 
		1-> get_value_from_sobject_xml(Response);
		_ ->F=fun(SobjectDescription)->get_value_from_sobject_xml(SobjectDescription) end,lists:map(F, Response)
	    end
		    
    end.

    
get_describe_sobjects_message([Type|Rest], Message)->
    get_describe_sobjects_message(Rest,lists:append([Message,"<sObjectType>", Type, "</sObjectType>"]));
get_describe_sobjects_message([], Message)->
    Message.


%OPERATION: describeGlobal
describe_global(SessionId, Endpoint)->
    get_value_from_sobject_xml(send_sforce_soap_message("<describeGlobal xmlns=\"urn:partner.soap.sforce.com\"/>", SessionId, Endpoint)).


%OPERATION: describeDataCategoryGroups
describe_data_category_groups(Type, SessionId, Endpoint)->
    DescribeDataCategoryGroupsMessage=lists:append(["<describeDataCategoryGroups xmlns=\"urn:partner.soap.sforce.com\"><sObjectType>", Type, "</sObjectType></describeDataCategoryGroups>"]),
    send_sforce_soap_message(DescribeDataCategoryGroupsMessage, SessionId, Endpoint).

%OPERATION: describetabs
describe_tabs(SessionId, Endpoint)->
    DescribeTabsMessage="<describeTabs xmlns=\"urn:partner.soap.sforce.com\"/>",
    [{label,[],[Label]},{logoUrl,[],[LogoUrl]},{namespace,[],NameSpace},{selected,[],[Selected]}|Tabs]=send_sforce_soap_message(DescribeTabsMessage, SessionId, Endpoint),
    FlattenedTabs=flatten_tabs(Tabs),
    [{"Label", Label},{"LogoUrl", LogoUrl},{"NameSpace",NameSpace},{"Selected", Selected},{"Tabs",FlattenedTabs}].
flatten_tabs(Tabs)->
    InnerFlatten=fun(TabData)->
			 {Name,_,Value}=TabData,
			 NameString=atom_to_list(Name),
			 case Value of
			     []->{NameString,""}; 
			     _ ->ValueString=lists:flatten(Value),{NameString,ValueString}
			 end
		 end,
    OuterFlatten=fun(Tab)->{tabs,[],TabData}=Tab,lists:map(InnerFlatten,TabData) end,
    lists:map(OuterFlatten, Tabs).
	      

%OPERATION: describeSoftphoneLayout
describe_softphone_layout(SessionId, Endpoint)->
    DescribeSoftphoneLayoutMessage="<describeSoftphoneLayout xmlns=\"urn:partner.soap.sforce.com\"/>",
    send_sforce_soap_message(DescribeSoftphoneLayoutMessage, SessionId, Endpoint).


%OPERATION: describeLayout
describe_layout(Type, SessionId, Endpoint)->
    DescribeLayoutMessage=lists:append(["<describeLayout xmlns=\"urn:partner.soap.sforce.com\"><sObjectType>", Type, "</sObjectType></describeLayout>"]),
    send_sforce_soap_message(DescribeLayoutMessage, SessionId, Endpoint).

%OPERATION: describeLayout
describe_layout(Type, RecordTypeIds,SessionId, Endpoint)->
    DescribeLayoutMessage=lists:append(["<describeLayout xmlns=\"urn:partner.soap.sforce.com\"><sObjectType>", Type, "</sObjectType><recordTypeIds/></describeLayout>"]),
    send_sforce_soap_message(DescribeLayoutMessage, SessionId, Endpoint).


%OPERATION: upsert

upsert(ExternalIdFieldName, Sobjects,SessionId, Endpoint)->
    UpsertMessage=lists:append(["<upsert xmlns=\"urn:partner.soap.sforce.com\"><externalIDFieldName>", ExternalIdFieldName,"</externalIDFieldName>",get_xml_for_sobjects(Sobjects),"</upsert>"]),
    Results=send_sforce_soap_message(UpsertMessage, SessionId, Endpoint),
    
    case Results of 
	[{_,_,_}|_]->
	    convert_xml_to_tuples(Results);
	_ ->ConvertUpsertResults=fun(Result)->convert_xml_to_tuples(Result) end,lists:map(ConvertUpsertResults,Results)
    end.
			  
%OPERATION: merge

merge(MasterRecordType, MasterRecordId, IdsToMerge, SessionId, Endpoint)->    
    MergeMessage=lists:append(["<merge xmlns=\"urn:partner.soap.sforce.com\" xmlns:sobj=\"urn:sobject.partner.soap.sforce.com\"><request><masterRecord><sobj:type>", MasterRecordType, "</sobj:type><sobj:Id>",MasterRecordId,"</sobj:Id></masterRecord>", get_ids_to_merge_xml(IdsToMerge), "</request></merge>"]),
    Results=send_sforce_soap_message(MergeMessage, SessionId, Endpoint),
    case Results of 
	[{errors,_,_}|_]->get_merge_error(Results);
	_->get_merge_results(Results)
    end.

get_merge_error(Results)->
    [{errors,_, [{message,_,[Message]}, _]}|  _]=Results,
    {err, Message}.

get_ids_to_merge_xml(IdsToMerge)->
    get_ids_to_merge_xml(IdsToMerge,[]).

get_ids_to_merge_xml([H|T], Ids)->
    MyIds=lists:append([Ids, "<recordToMergeIds>", H, "</recordToMergeIds>"]),
    get_ids_to_merge_xml(T, MyIds);
get_ids_to_merge_xml([],Ids) ->
    Ids.

get_merge_results(Results)->    
    MergedId=get_property(Results,id),
    MergedRecords=get_atom_records(Results, mergedRecordIds),
    RelatedUpdatedRecords=get_atom_records(Results, updatedRelatedIds),
    Succeded=get_property(Results,success),
    
    [{"success", Succeded},{"id", MergedId},{"mergedRecords", MergedRecords},{"updatedRecords", RelatedUpdatedRecords}].

get_property([H|T], Property)->
    case H of
	{Property,_,[Value]}->
	    Value;
	_ ->get_property(T, Property)
    end;
get_property([], _)->
    err.

get_atom_records(Results, Label)->
    get_atom_records(Results,[],Label).

get_atom_records([H|T], Records, Label)->
    case H of
	{Label,_,[Id]}->
	    get_atom_records(T, lists:append(Records, [Id]), Label);
	_ -> get_atom_records(T, Records, Label)
    end;
get_atom_records([], Records, _) ->
    Records.
					


%OPERATION: logout

logout(SessionId, Endpoint)->
    SessionHeader=create_session_header(SessionId),
    LogoutBody="<logout xmlns=\"urn:partner.soap.sforce.com\"/>",
    LogoutSoapMessage=create_soap_envelope(create_soap_header(SessionHeader), create_soap_body(LogoutBody)),
    SoapResponse=send_soap_message(LogoutSoapMessage, Endpoint),
    Xml=parse_xml(SoapResponse),
    BodyXml=get_body_from_envelope(Xml),
    LogoutResponse=get_body_content(BodyXml),
    case LogoutResponse of
	{logoutResponse,[],[]}->
	    ok;
	_ ->err
    end.


%OPERATION: search

search(Search, SessionId, Endpoint)->
    SearchMessage=lists:append(["<search xmlns=\"urn:partner.soap.sforce.com\"><searchString>", Search, "</searchString></search>"]),
    Results=send_sforce_soap_message(SearchMessage, SessionId, Endpoint),
    F=fun(SearchRecords)-> {searchRecords,_,[{record,_,Record}]}=SearchRecords,get_value_from_sobject_xml(Record) end,
    lists:map(F, Results).



    





%OPERATION: setPassword

set_password(UserId, Password, SessionId, Endpoint)->
    SetPasswordMessage=lists:append(["<setPassword xmlns=\"urn:partner.soap.sforce.com\"><userId>", UserId ,"</userId><password>", Password, "</password></setPassword>"]),
    Results=send_sforce_soap_message(SetPasswordMessage, SessionId, Endpoint),
    case Results of 
	[]->
	    ok;
	_ -> Results
    end.


%OPERATION: resetPassword

reset_password(UserId, SessionId, Endpoint)->
    ResetPasswordMessage=lists:append(["<resetPassword xmlns=\"urn:partner.soap.sforce.com\"><userId>", UserId ,"</userId></resetPassword>"]),
    Results=send_sforce_soap_message(ResetPasswordMessage, SessionId, Endpoint),
    case Results of 
	[{password,[],[Password]}]->Password;
	_ -> Results
    end.

%OPERATION: sendEmail

send_email(Messages, SessionId, Endpoint)->
    SendEmailMessage=lists:append(["<sendEmail xmlns=\"urn:partner.soap.sforce.com\">", get_xml_for_messages(Messages), "</sendEmail>"]),
    Results=send_sforce_soap_message(SendEmailMessage, SessionId, Endpoint),
    F=fun(Result)->
	      case Result of 
		  [
		   {errors,[],
		    [{message,[],[ErrMessage]},
		     {statusCode,[],[_]},
		     _
		    ]
		   },
		   {success,[],["false"]}] ->
		      {err, ErrMessage};
		  [{success,[],["true"]}] ->ok
	      end
      end,
    lists:map(F, Results).





get_xml_for_messages(Messages)->
    get_xml_for_messages(Messages, []).

get_xml_for_messages([H|T], Xml)->
    NewXml=lists:append([Xml, "<messages xsi:type=\"SingleEmailMessage\">", get_xml_for_message(H), "</messages>"]),
    get_xml_for_messages(T, NewXml);
   
get_xml_for_messages([], Xml) ->
    Xml.

get_xml_for_message(Message)->
    get_xml_for_message(Message, []).


% this is pretty horrible...

get_xml_for_message([H|T], Xml)->
    {ParamName, ParamValue}=H,
    case ParamName of
	bccAddress->get_xml_for_message(T, lists:append([Xml, "<bccAddresses>", ParamValue, "</bccAddresses>"]));
	bccAddresses-> F = fun(Address) -> lists:append(["<bccAddresses>",Address,"</bccAddresses>"]) end, get_xml_for_message(T,lists:append([Xml,lists:flatten(lists:map(F,ParamValue))]));
	ccAddress->get_xml_for_message(T, lists:append([Xml, "<ccAddresses>", ParamValue, "</ccAddresses>"]));
	ccAddresses-> F = fun(Address) -> lists:append(["<ccAddresses>",Address,"</ccAddresses>"]) end, get_xml_for_message(T,lists:append([Xml,lists:flatten(lists:map(F,ParamValue))]));
	documentAttachment->get_xml_for_message(T, lists:append([Xml, "<documentAttachments>", ParamValue, "</documentAttachments>"]));
	documentAttachments->F = fun(Id) -> lists:append(["<documentAttachments>",Id,"</documentAttachments>"]) end, get_xml_for_message(T,lists:append([Xml,lists:flatten(lists:map(F,ParamValue))]));
	toAddress->get_xml_for_message(T, lists:append([Xml, "<toAddresses>", ParamValue, "</toAddresses>"]));
	toAddresses-> F = fun(Address) -> lists:append(["<toAddresses>",Address,"</toAddresses>"]) end, get_xml_for_message(T,lists:append([Xml,lists:flatten(lists:map(F,ParamValue))]));
	bccSender-> get_xml_for_message(T, lists:append([Xml, "<", atom_to_list(ParamName), ">", atom_to_list(ParamValue), "</", atom_to_list(ParamName), ">"]));
	saveAsActivity-> get_xml_for_message(T, lists:append([Xml, "<", atom_to_list(ParamName), ">", atom_to_list(ParamValue), "</", atom_to_list(ParamName), ">"]));
	useSignature-> get_xml_for_message(T, lists:append([Xml, "<", atom_to_list(ParamName), ">", atom_to_list(ParamValue), "</", atom_to_list(ParamName), ">"]));
	targetObjectId->get_xml_for_message(T, lists:append([Xml, "<targetObjectIds>", ParamValue, "</targetObjectIds>"]));
	targetObjectIds->F = fun(Id) -> lists:append(["<targetObjectIds>",Id,"</targetObjectIds>"]) end, get_xml_for_message(T,lists:append([Xml,lists:flatten(lists:map(F,ParamValue))]));
	whatId->get_xml_for_message(T, lists:append([Xml, "<whatIds>", ParamValue, "</whatIds>"]));
	whatIds->F = fun(Id) -> lists:append(["<whatIds>",Id,"</whatIds>"]) end, get_xml_for_message(T,lists:append([Xml,lists:flatten(lists:map(F,ParamValue))]));
	fileAttachment->get_xml_for_message(T, lists:append([Xml, "<fileAttachments>", get_xml_for_file_attachment(ParamValue), "</fileAttachments>"]));
	fileAttachments->F = fun(FileAttachment) -> lists:append(["<fileAttachments>",get_xml_for_file_attachment(FileAttachment),"</fileAttachments>"]) end, get_xml_for_message(T,lists:append([Xml,lists:flatten(lists:map(F,ParamValue))]));
	
	_ -> get_xml_for_message(T, lists:append([Xml, "<", atom_to_list(ParamName), ">", ParamValue, "</", atom_to_list(ParamName), ">"]))
    end;    
get_xml_for_message([], Xml) ->
    Xml.
    
get_xml_for_file_attachment(FileAttachmentList)->
    get_xml_for_file_attachment(FileAttachmentList,[]).

get_xml_for_file_attachment([H|T],Xml)->  
    {ParamName, ParamValue}=H,
    case ParamName of 
	inline->get_xml_for_file_attachment(T, lists:append([Xml, "<", atom_to_list(ParamName), ">", atom_to_list(ParamValue), "</", atom_to_list(ParamName), ">"]));
	 _-> get_xml_for_message(T, lists:append([Xml, "<", atom_to_list(ParamName), ">", ParamValue, "</", atom_to_list(ParamName), ">"]))
    end;
get_xml_for_file_attachment([], Xml) ->
	    Xml.
    

%send_mass_email(ToAddresses, CcAddresses, BccAddresses, BccSender, SenderDisplayName, Subject, PlainTextBody, HtmlBody, EmailPriority, ReplyTo, SaveAsActivity, UseSignature, TemplateId, Charset, DocumentAttachments, References, WhatId)->





%OPERATION: convertLead

convert_lead(LeadId, ContactId, AccountId, OwnerId, OverWriteLeadSource, DoNotCreateOpportunity, OpportunityName, ConvertedStatus, SendEmailToOwner, SessionId, Endpoint)->
    ConvertLeadMessage=lists:append(["<convertLead xmlns=\"urn:partner.soap.sforce.com\"><leadConverts><accountId>", AccountId ,"</accountId><contactId>", ContactId,"</contactId><convertedStatus>",ConvertedStatus,"</convertedStatus><doNotCreateOpportunity>", DoNotCreateOpportunity, "</doNotCreateOpportunity><leadId>",LeadId,"</leadId>", get_opportunity_name(OpportunityName),"<overwriteLeadSource>",OverWriteLeadSource,"</overwriteLeadSource><ownerId>",OwnerId,"</ownerId><sendNotificationEmail>",SendEmailToOwner,"</sendNotificationEmail></leadConverts></convertLead>"]),
    Results=send_sforce_soap_message(ConvertLeadMessage, SessionId, Endpoint),
    

    % really needs to be a better way to do this...
    case Results of 
	[{accountId,_,_},
	 {contactId,_,_},
	 {errors,_, Errors},
	 {leadId,_,_},
	 {opportunityId,_,_},
	 {success,_,["false"]}] -> [{message,[],[ErrMessage]}|_]=Errors,
				   {err, ErrMessage};
	[{accountId,_,ReturnedAccountId},
	 {contactId,_,ReturnedContactId},
	 {leadId,_,ReturnedLeadId},
	 {opportunityId,_,ReturnedOpportunityId},
	 {success,_,["true"]}] -> [{"accountId", lists:flatten(ReturnedAccountId)},{"contactId", lists:flatten(ReturnedContactId)},{"leadId", lists:flatten(ReturnedLeadId)},{"opportunityId", lists:flatten(ReturnedOpportunityId)}]
    end.


get_opportunity_name(OpportunityName)->
    case OpportunityName of 
	[]->"";
	_->lists:append(["<opportunityName>",OpportunityName,"</opportunityName>"])
    end.


%OPERATION: process_submit
process_submit(Id, Comment, NextApproverIds,SessionId, Endpoint)->
    ProcessMessage=get_process_envelope("ProcessSubmitRequest", lists:append(["<objectId>", Id, "</objectId>"]), Comment, NextApproverIds),
    send_process_request(ProcessMessage, SessionId, Endpoint).

send_process_request(Request, SessionId, Endpoint)->
    Results=send_sforce_soap_message(Request, SessionId, Endpoint),
    case Results of
	[_,{errors,[],
                         [{message,_,
                              [ErrorMessage]},
                          {statusCode,_,[_]}]}|_]->
	    {err, ErrorMessage};
	_->
	    get_process_response(Results, [],[],[])
    end.

get_process_response([H|T], Xml, NewWorkitemIds, ActorIds)->
    case H of
	{actorIds,[],[ActorId]}->
	    get_process_response(T, Xml,NewWorkitemIds,lists:append(ActorIds, [ActorId]));
	{newWorkitemIds,[],[NewWorkItemId]} ->
	    get_process_response(T, Xml,lists:append(NewWorkitemIds, [NewWorkItemId]), ActorIds);
	{Name,_,[Value]}->
	    get_process_response(T, lists:append([Xml, [{atom_to_list(Name), Value}]]),NewWorkitemIds, ActorIds)
    end;
get_process_response([], Xml, NewWorkitemIds, ActorIds)->
    lists:append([Xml, [{"newWorkitemIds", NewWorkitemIds}], [{"actorIds", ActorIds}]]).

%OPERATION: process_workitem
process_workitem(WorkItemId, Action, Comment, NextApproverIds,SessionId, Endpoint)->
    ProcessMessage=get_process_envelope("ProcessWorkitemRequest", lists:append(["<workitemId>", WorkItemId, "</workitemId><action>", Action, "</action>"]), Comment, NextApproverIds),
    send_process_request(ProcessMessage, SessionId, Endpoint).



get_process_envelope(ProcessRequestType, Xml, Comment, NextApproverIds)->
    lists:append(["<process xmlns=\"urn:partner.soap.sforce.com\"><actions xsi:type=\"tns:", ProcessRequestType, "\"><comments>",Comment,"</comments>",get_xml_for_next_approver_ids(NextApproverIds),Xml,"</actions></process>"]).
     
get_xml_for_next_approver_ids(NextApproverIds)->
    get_xml_for_next_approver_ids("", NextApproverIds).

get_xml_for_next_approver_ids(Xml, [H|T])->
    ApproverXml=lists:append([Xml, "<nextApproverIds>",H,"</nextApproverIds>"]),
    get_xml_for_next_approver_ids(ApproverXml, T);
get_xml_for_next_approver_ids(Xml,[]) ->
    Xml.




%OPERATION: invalidateSessions 
invalidate_sessions(SessionIdsToInvalidate, SessionId, Endpoint)->
    InvalidateMessage=lists:append(["<invalidateSessions xmlns=\"urn:partner.soap.sforce.com\">", get_xml_for_session_ids(SessionIdsToInvalidate),"</invalidateSessions>"]),
    Results=send_sforce_soap_message(InvalidateMessage, SessionId, Endpoint),
   case  Results of
       [{success,[],["true"]}]->ok;
       [{errors,[],
	 [{message,_,
	   [ErrorMessage]},
	  {statusCode,_,[_]}]
	}|_]->
	   {err, ErrorMessage}
   end.


%OPERATION: emptyRecycleBin
empty_recycle_bin(Ids, SessionId, Endpoint)->
    object_id_operation("emptyRecycleBin", Ids, SessionId, Endpoint).


%OPERATION: undelete
undelete(Ids, SessionId, Endpoint)->
    object_id_operation("undelete", Ids, SessionId, Endpoint).

object_id_operation(Operation, Ids, SessionId, Endpoint)->
    XmlForIds=fun(Id)->lists:append(["<Ids>", Id,"</Ids>"]) end,
    EmptyRecycleBinMessage=lists:append(["<", Operation , " xmlns=\"urn:partner.soap.sforce.com\">", lists:flatten(lists:map(XmlForIds, Ids)), "</", Operation, ">"]),
    Results=send_sforce_soap_message(EmptyRecycleBinMessage, SessionId, Endpoint),
    ConvertResults=fun(Result)->
			   case Result of
			       [{id,[],[EmptiedId]},{success,[],["true"]}]->{ok, EmptiedId};
			       [{errors,[],
				 [{message,_,
				   [ErrorMessage]},
				  {statusCode,_,[_]}]
				}|_]->{err, ErrorMessage}
			   end
		   end,
    lists:map(ConvertResults,Results).



%OPERATION: retrieve
retrieve(Fields, Type, Ids, SessionId, Endpoint)->
    XmlForIds=fun(Id)->lists:append(["<ids>", Id,"</ids>"]) end,
    ConcatenateFields=fun(MyField)->lists:append([",", MyField]) end,
    [Field|Rest]=Fields,
    RetrieveMessage=lists:append(["<retrieve xmlns=\"urn:partner.soap.sforce.com\"><fieldList>",Field, lists:flatten(lists:map(ConcatenateFields,Rest)),"</fieldList><sObjectType>", Type, "</sObjectType>", lists:flatten(lists:map(XmlForIds, Ids)), "</retrieve>"]),
    Results=send_sforce_soap_message(RetrieveMessage, SessionId, Endpoint),
    case Results of
	{err, _}->
	    Results;
	_ ->F=fun(Result)-> convert_xml_to_sobject(Result,[]) end,
	    lists:map(F, Results)
    end.
			    

get_xml_for_session_ids(SessionIds)->
    get_xml_for_session_ids(SessionIds, []).

get_xml_for_session_ids([H|T], Xml)->
    get_xml_for_session_ids(T, lists:append([Xml, "<sessionIds>", H, "</sessionIds>"]));
get_xml_for_session_ids([], Xml) ->
    Xml.
    


%OPERATION: getUpdated
get_updated(ObjectType, StartDate, EndDate, SessionId, Endpoint)->
    GetUpdatedMessage=lists:append(["<getUpdated xmlns=\"urn:partner.soap.sforce.com\"><sObjectType>",ObjectType,"</sObjectType><startDate>",erlang_date_to_xsd_date_time(StartDate),"</startDate><endDate>",     erlang_date_to_xsd_date_time(EndDate),"</endDate></getUpdated>"]),
   Results=send_sforce_soap_message(GetUpdatedMessage, SessionId, Endpoint),
    get_updated_results(Results).
    

get_updated_results(Results)->
    get_updated_results(Results,[], []).

get_updated_results([H|T],Ids, LastDate)->
    case H of 
	{ids,[],[Id]}->
	   get_updated_results(T,lists:append(Ids, [Id]), LastDate);
	 {latestDateCovered,_, [LastDateCovered]}-> 
	    get_updated_results(T,Ids, xsd_datetime_to_erlang_datetime(LastDateCovered))
    end;
get_updated_results([], Ids, LastDate) ->
    {{ids, Ids},{lastDateCovered, LastDate}}.

									

%SObject

get_xml_for_sobjects(Sobjects)-> 
    get_xml_for_sobjects(Sobjects,[]). 

get_xml_for_sobjects([H|T],Xml)-> 
    MyXml=lists:append([Xml,"<sObjects xmlns:sobj=\"urn:sobject.partner.soap.sforce.com\">",get_xml_for_sobject(H), "</sObjects>"]), 
    get_xml_for_sobjects(T, MyXml);
get_xml_for_sobjects([],Xml) ->
    Xml.

get_xml_for_sobject(Sobject)->
 get_xml_for_sobject(Sobject,[]).
 
get_xml_for_sobject([H|T],Xml)-> 
    {Name,Type,Value}=H,
    get_xml_for_sobject(T, lists:append([Xml, "<sobj:", Name, " xsi:type=\"", lists:append(["xsd:",Type]), "\">", Value, "</sobj:", Name, ">"]));
get_xml_for_sobject([],Xml) ->
    Xml.


convert_xml_to_sobject([H|T], Sobject)->
    
    case H of 
    {Name,_,Values}->
	    MySobject=lists:append(Sobject,[{clean_prefix(atom_to_list(Name)), "string", get_value_from_sobject_xml(Values)}]),
	    convert_xml_to_sobject(T, MySobject);
	_-> lists:flatten([H,T])
	    
    end;	    
convert_xml_to_sobject([], Sobject)->    
   Sobject. 

clean_prefix(Name)->
    PrefixEnd=string:str(Name, ":"),
    case PrefixEnd of 
	0->Name;
	_-> string:substr(Name,1+PrefixEnd)
    end.



get_value_from_sobject_xml(XmlValue)->
    case length(XmlValue) of
	1 -> [Value]=XmlValue,Value;
	0 ->[];	     
	_ -> convert_xml_to_sobject(XmlValue,[])
    end.


get_tuples_from_xml(XmlValue)->
    case length(XmlValue) of
	1 -> [Value]=XmlValue,Value;
	0 ->[];	     
	_ -> convert_xml_to_tuples(XmlValue,[])
    end.


	    
convert_xml_to_tuples(XmlValue)->
    convert_xml_to_tuples(XmlValue,[]).
    
convert_xml_to_tuples([H|T], Xml)->
    {Name,_,Values}=H,
    MyTuples=lists:append(Xml,[{clean_prefix(atom_to_list(Name)),  get_tuples_from_xml(Values)}]),
    convert_xml_to_tuples(T, MyTuples);
convert_xml_to_tuples([], Xml)->    
   Xml.    

%% XML support

create_xml_declaration () ->
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>".


parse_xml(Message)->
    {Xml, _}=xmerl_scan:string(Message),
    xmerl_lib:simplify_element(Xml).
    
is_namespace_prefixed_element(SimplifiedXml, UnqualifiedName) ->			
    {QualifiedName, _, _}=SimplifiedXml,
    ReversedUnqualifiedName=lists:append([lists:reverse(UnqualifiedName), ":"]),
    Reversed=lists:reverse(atom_to_list(QualifiedName)),
    case (lists:prefix(ReversedUnqualifiedName, Reversed)) of
	true->ok;
	false->err
    end.

erlang_date_to_xsd_date_time(ErlangDate) ->
      {{YearInt, MonthInt,DayInt},{HourInt, MinutesInt, SecondsInt}}=ErlangDate,
    lists:append([integer_to_list(YearInt), "-", integer_pad(MonthInt), "-", integer_pad(DayInt), "T", integer_pad(HourInt), ":", integer_pad(MinutesInt), ":", integer_pad(SecondsInt), "Z"]).
    
xsd_datetime_to_erlang_datetime(XsdDateString)->
    [Year, Month, Day, Hour, Minutes, Seconds,_]=string:tokens(XsdDateString, "-T:."),
    {YearInt,_}=string:to_integer(Year),
    {MonthInt,_}=string:to_integer(Month),
    {DayInt,_}=string:to_integer(Day),
    {HourInt,_}=string:to_integer(Hour),
    {MinutesInt,_}=string:to_integer(Minutes),
    {SecondsInt,_}=string:to_integer(Seconds),
    {{YearInt, MonthInt,DayInt},{HourInt, MinutesInt, SecondsInt}}.


integer_pad(Integer)->
    [IntegerString, _]=io_lib:format("~2..0B~n", [Integer]),
    lists:flatten(IntegerString).

%% SOAP Support

create_soap_envelope (SoapHeader, SoapBody) ->
    lists:append(["<soap:Envelope xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\">", SoapHeader, SoapBody, "</soap:Envelope>"]). 

create_soap_header(Xml)->
    lists:append(["<soap:Header>",Xml,"</soap:Header>"]).
    
create_soap_body(Xml) ->
    lists:append(["<soap:Body>", Xml, "</soap:Body>"]).

create_login_block (Username, Password) ->
    lists:append(["<login xmlns=\"urn:partner.soap.sforce.com\"><username>", Username, "</username><password>", Password, "</password></login>"]).

get_body_from_envelope(EnvelopeXml)->
    {_, _, ChildElements}=EnvelopeXml,
    [BodyElement]=ChildElements,
    case (is_namespace_prefixed_element(BodyElement, "Body")) of
	 ok -> BodyElement; 
	 err -> err
	end.
		
get_fault(BodyXml)->
    FaultElement=get_body_content(BodyXml),
    {_,_,FaultChildElements}=FaultElement,
    [FaultCode, FaultString|Detail]=FaultChildElements,
    {faultcode,_,[FaultCodeValue]}=FaultCode,
    {faultstring,_,FaultMessage}=FaultString,
%%    io:fwrite("message ~s ~n", ["here..."]),
    case Detail of
	[]->[{faultcode, FaultCodeValue},{faultstring, lists:flatten(FaultMessage)}];%,{detail,FaultDetail}]
	{detail,_,[FaultDetail]} -> extract_fault_detail(FaultDetail);
	[{detail,[],[FaultDetail]}] -> extract_fault_detail(FaultDetail)
    end.
    
extract_fault_detail(FaultDetail)->
    {'sf:InvalidIdFault',
     [{'xsi:type',"sf:InvalidIdFault"}],
     [{'sf:exceptionCode',[],[ExceptionCode]},
      {'sf:exceptionMessage',[],
       [ExceptionMessage]}]}=FaultDetail,
    [{errorCode, ExceptionCode}, {errorMessage, ExceptionMessage}].

get_body_content(BodyXml)->
    {_,_,ChildElements}=BodyXml,
    [BodyContent]=ChildElements,
    BodyContent.

is_fault(BodyElement) ->
   {_, _, ChildElements}=BodyElement,
    [ChildElement]=ChildElements,
    is_namespace_prefixed_element(ChildElement,"Fault").

is_soap(SimplifiedXml) ->
   is_namespace_prefixed_element(SimplifiedXml,"Envelope").


send_soap_message(SoapMessage, Endpoint)->
    {ok, {{_, _, _}, _, ResponseBody}}=httpc:request(post, {Endpoint, [{"SOAPAction:", "\"\""}], "text/xml", SoapMessage}, [],[]),
    ResponseBody.


% SForce Soap Utilities

send_sforce_soap_message(SforceXml, SessionId, Endpoint)->
    SessionHeader=create_session_header(SessionId),
    SoapMessage=create_soap_envelope(create_soap_header(SessionHeader), create_soap_body(SforceXml)),
    SoapResponse=send_soap_message(SoapMessage, Endpoint),
  %  io:fwrite("message ~s ~n", [SoapResponse]),
    Xml=parse_xml(SoapResponse),
    BodyXml=get_body_from_envelope(Xml),
    case is_fault(BodyXml) of
        ok -> [_, {_,ErrorMessage}]=get_fault(BodyXml), {err, ErrorMessage};
        _ -> get_results_from_message(get_body_content(BodyXml))
    end.
 

get_results_from_message(Body)->
    {_,[],Results}=Body,
    case length(Results) of 
	1->[{_,_,Result}]=Results,Result;
	_ -> F=fun({_,_,Result}) -> Result end,
	     lists:map(F,Results)
    end.
	   

    
% Copyright (c) 2010, Ian Brown
% All rights reserved.

% Redistribution and use in source and binary forms,
% with or without modification,
% are permitted provided that the following conditions are met:

% Redistributions of source code must retain the above copyright notice,
% this list of conditions and the following disclaimer.
% Redistributions in binary form must reproduce the above copyright notice,
% this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
% Neither the name of ErlForce, nor the names of its contributors may be used to endorse or promote products derived from this 
% software without specific prior written permission.
% 
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
% IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, 
% STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
