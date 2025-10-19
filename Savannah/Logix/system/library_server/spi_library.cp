/*
** This module is part of EFCP.
**

     Copyright 2007 William Silverman
     Weizmann Institute of Science, Rehovot, Israel

** EFCP is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 2 of the License, or
** (at your option) any later version.
** 
** EFCP is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the
** GNU General Public License for more details.
** 
** You should have received a copy of the GNU General Public License
** along with EFCP; if not, see:

       http://www.gnu.org/licenses

** or write to:

       Free Software Foundation, Inc.
       51 Franklin Street, Fifth Floor
       Boston, MA 02110-1301 USA

       contact: bill@wisdom.weizmann.ac.il

**
*/

/*
       Spifcp Library processes
       1998
*/

-export([string/1]).
-mode(trust).
-language(compound).

procedure string(String).

string(Text) :- true : Text =

"library.
-language([evaluate,compound,colon]).
-include(spi_constants).

spi_channel(Channel) :-
	spi_channel(Channel, 'SYSTEM', infinite).
spi_channel(Channel, Creator) :-
	spi_channel(Channel, Creator, infinite).
spi_channel(Channel, Creator, BaseRate) :-
    number(BaseRate) :
      Channel = Channel'? |
	spi_monitor#new_channel(Creator, Channel', BaseRate);
    BaseRate =?= infinite :
      Channel = Channel'? |
	spi_monitor#new_channel(Creator, Channel', BaseRate);
    otherwise :
      Creator = _,
      BaseRate = _ |
	computation#display(('spi_library: Can''t make channel' : Channel)).

spi_channel(Channel, Creator, BaseRate, ComputeWeight) :-
    string(ComputeWeight) :
      Channel = Channel'? |
	spi_monitor#new_channel(Creator, Channel', ComputeWeight, BaseRate);
    tuple(ComputeWeight) :
      Channel = Channel'? |
	spi_utils#make_channel(Creator, Channel', ComputeWeight, BaseRate);
    otherwise :
      Creator = _,
      BaseRate = _,
      ComputeWeight = _ |
	computation#display(('spi_library: Can''t make channel' : Channel)).

spi_send(Message, Channel) :-
	spi_send(Message, Channel, 1, sender).
spi_send(Message, Channel, Multiplier) :-
	spi_send(Message, Channel, Multiplier, sender).
spi_send(Message, Channel, Multiplier, Id) :-
    vector(Channel) :
      Send = SPI_SEND(Id, Channel, Multiplier, 1) |
	spi_monitor#scheduler(S),
	write_channel(start(spi_send, [Send], Value, Chosen), S),
	spi_transmitted(sending(Id), 1, Chosen, Message, Value);
    otherwise :
      Id = _,
      Message = _,
      Multiplier = _ |
	computation#display(('spi_library: Can''t send to' : Channel)).

spi_receive(Channel, Message) :-
	spi_receive(Channel, Message, 1, receiver).
spi_receive(Channel, Message, Multiplier) :-
	spi_receive(Channel, Message, Multiplier, receiver).
spi_receive(Channel, Message, Multiplier, Id) :-
    vector(Channel) :
      Receive = SPI_RECEIVE(Id, Channel, Multiplier, 2) |
	spi_monitor#scheduler(S),
	write_channel(start(spi_receive, [Receive], Value, Chosen), S),
	spi_transmitted(receiving(Id), 2, Chosen, Message, Value?);
    otherwise :
      Id = _,
      Message = _,
      Multiplier = _ |
	computation#display(('spi_library: Can''t receive from' : Channel)).

spi_close(Channel) :-
    vector(Channel) |
	spi_monitor#scheduler(S),
	write_channel(close({Channel}), S?);
    otherwise |
	computation#display(('spi_library: Can''t close' : Channel)).

spi_transmitted(Id, Tag, Chosen, Message, Value) :-

    Chosen = Tag :
      Id = _,
      Value = Message;

    Chosen =\= Tag :
      Message = _,
      Id = _,
      Value = _.

spi_update_channel_refs(List, S1, S2) :-

    List ? Add,
    Add =?= Increment(Channel),
    integer(Increment),
    vector(Channel),
    arity(Channel, CHANNEL_SIZE),
    read_vector(SPI_CHANNEL_REFS, Channel, Refs),
    Refs += Increment :
      store_vector(SPI_CHANNEL_REFS, Refs', Channel) |
	self;

    List ? Close,
    Close = close(Channels),
    tuple(Channels) :
      List' = _,
      write_channel(Close, S1, S2);

    List =?= [] :
      S2 = S1.

set_base_rate(Rate, Channels, Reply) :-

    convert_to_real(Rate, RealRate), RealRate > 0,
    Channels ? Channel,
    vector(Channel),
    arity(Channel, CHANNEL_SIZE),
    read_vector(SPI_CHANNEL_TYPE, Channel, Type),
    bitwise_and(Type, SPI_TYPE_MASK, MaskedType),
    MaskedType =\= SPI_CHANNEL_ANCHOR,
    MaskedType =\= SPI_INSTANTANEOUS,
    MaskedType =\= SPI_SINK :
      store_vector(SPI_CHANNEL_RATE, RealRate, Channel) |
	self;

    Channels =?= [] :
      Rate = _,
      Reply = true;

    otherwise :
      Rate = _,
      Reply = false(Rate, Channels).

randomize_messages(Channels, Reply) :-
 
    Channels ? Channel,
    vector(Channel),
    arity(Channel, CHANNEL_SIZE),
    read_vector(SPI_CHANNEL_TYPE, Channel, Type),
    bitwise_or(Type, SPI_RANDOM_FLAG, RandomizedType) :
      store_vector(SPI_CHANNEL_TYPE, RandomizedType, Channel) |
	self;

    Channels = [] :
      Reply = true;

    otherwise :
      Reply = false(Channels).

serialize_messages(Channels, Reply) :-
 
   Channels ? Channel,
    vector(Channel),
    arity(Channel, CHANNEL_SIZE),
    read_vector(SPI_CHANNEL_TYPE, Channel, Type),
    bitwise_not(SPI_RANDOM_FLAG, Mask),
    bitwise_and(Type, Mask, SerializedType) :
      store_vector(SPI_CHANNEL_TYPE, SerializedType, Channel) |
	self;

    Channels = [] :
      Reply = true;

    otherwise :
      Reply = false(Channels).

get_channel_status(Channel, Attribute, Value) :-

    Attribute =?= blocked,
    read_vector(SPI_BLOCKED, Channel, Blocked),
    Blocked =?= 0 :
      Value = false;

    Attribute =?= blocked,
    read_vector(SPI_BLOCKED, Channel, Blocked),
    Blocked =\= 0 :
      Value = true;

    Attribute =?= type,
    read_vector(SPI_CHANNEL_TYPE, Channel, Type),
    bitwise_and(Type, SPI_RANDOM_FLAG, 0) :
      Value = TypeName? |
	channel_type_name;

    Attribute =?= type,
    read_vector(SPI_CHANNEL_TYPE, Channel, Type),
    bitwise_and(Type, SPI_RANDOM_FLAG, SPI_RANDOM_FLAG) :
      Value = #TypeName? |
	channel_type_name;

    Attribute =?= baserate,
    read_vector(SPI_CHANNEL_RATE, Channel, Rate) :
      Value = Rate? ;

    Attribute =?= references,
    read_vector(SPI_CHANNEL_REFS, Channel, Refs) :
      Value = Refs? ;

   Attribute =?= messages,
   read_vector(SPI_CHANNEL_TYPE, Channel, Type),
   bitwise_and(Type, SPI_TYPE_MASK, SPI_HOMODIMERIZED),
   read_vector(SPI_DIMER_ANCHOR, Channel, Anchor) :
       Value = Count? |
	channel_messages + (Request = Anchor, Counter = 0);

   Attribute =?= messages,
   read_vector(SPI_CHANNEL_TYPE, Channel, Type),
   bitwise_and(Type, SPI_TYPE_MASK, BasicType),
   BasicType =\= SPI_HOMODIMERIZED :
     Value = {Sends?, Receives?} |
	get_channel_status(Channel, sends, Sends),
	get_channel_status(Channel, receives, Receives);

    Attribute =?= sends,
    read_vector(SPI_SEND_ANCHOR, Channel, Anchor) :
      Value = Count? |
	channel_messages + (Request = Anchor, Counter = 0);

    Attribute =?= receives,
    read_vector(SPI_RECEIVE_ANCHOR, Channel, Anchor) :
      Value = Count? |
	channel_messages + (Request = Anchor, Counter = 0);

    Attribute =?= weight,
    read_vector(SPI_CHANNEL_TYPE, Channel, Type),
    bitwise_and(Type, SPI_TYPE_MASK, SPI_HOMODIMERIZED),
    read_vector(SPI_CHANNEL_RATE, Channel, BR),
    read_vector(SPI_DIMER_WEIGHT, Channel, DW),
    Weight := BR*DW*(DW-1) :
      Value = Weight;

    Attribute =?= weight,
    read_vector(SPI_CHANNEL_TYPE, Channel, Type),
    bitwise_and(Type, SPI_TYPE_MASK, T),
    T =\= SPI_HOMODIMERIZED,
    read_vector(SPI_CHANNEL_RATE, Channel, BR),
    read_vector(SPI_SEND_WEIGHT, Channel, SW),
    read_vector(SPI_RECEIVE_WEIGHT, Channel, RW),
    Weight :=  BR*SW*RW :
      Value = Weight;

    Attribute =?= name,
    read_vector(SPI_CHANNEL_NAME, Channel, Name) :
      Value = Name? ;
      
    Attribute ? messages |
	get_channel_status(Channel, messages, Reply),
	which_kind_of_messages(Reply?, Value, Value'),
	self;
      
    Attribute ? Item,
    Item =\= messages :
      Value ! (Item = Reply?) |
	get_channel_status(Channel, Item, Reply),
	self;

    Attribute =?= [] :
      Channel = _,
      Value = [].

  which_kind_of_messages(Messages, Value, Tail) :-
    Messages =?= {Sends, Receives} :
      Value = [sends(Sends?), receives(Receives?) | Tail];
    otherwise :
      Value = [dimers(Messages) | Tail].

  channel_type_name(Type, TypeName) :-

    bitwise_and(Type, SPI_TYPE_MASK, SPI_CHANNEL_ANCHOR) :
      TypeName = anchor;

    bitwise_and(Type, SPI_TYPE_MASK, SPI_UNKNOWN) :
      TypeName = based;

    bitwise_and(Type, SPI_TYPE_MASK, SPI_BIMOLECULAR) :
      TypeName = bimolecular;

    bitwise_and(Type, SPI_TYPE_MASK, SPI_HOMODIMERIZED) :
      TypeName = homodimerized;

    bitwise_and(Type, SPI_TYPE_MASK, SPI_INSTANTANEOUS) :
      TypeName = infinite;

    bitwise_and(Type, SPI_TYPE_MASK, SPI_SINK) :
      TypeName = sink;

    bitwise_and(Type, SPI_TYPE_MASK, BasicType),
    BasicType > SPI_SINK,
    convert_to_string(BasicType, NumericString) :
      TypeName = NumericString.

  channel_messages(Anchor, Request, Counter, Count) :-

    arg(SPI_MESSAGE_LINKS, Request, Links),
    read_vector(SPI_NEXT_MS, Links, Request'),
    Request' =\= Anchor,
    Request' =\= Request,
    Counter++ |
	self;

    otherwise :
      Anchor = _,
      Request = _,
      Count = Counter.

object(Name, InitialValue, Object) :-
    string(Name) |
	spi_monitor#public_object(Name, InitialValue, Object).

spi_object_request(Request, Object, PrimedObject) :-

    vector(Object),
    arity(Object, OBJECT_ARITY) :
      write_vector(OBJECT_REQUESTS, Request, Object, PrimedObject);

    otherwise :
      PrimedObject = _ |
	computation#display(('spi_library: Can''t send'(Object!Request))).

/************************** Compilation Helpers *****************************/

is_channel(CallId, ChannelName, Term) :-

    vector(Term),
    arity(Term, CHANNEL_SIZE) :
      CallId = _,
      ChannelName = _;

    otherwise |
	user_event(CallId, not_a_channel(ChannelName ? Term)).

isnt_channel(CallId, VariableName, Term) :-

    vector(Term),
    arity(Term, CHANNEL_SIZE) |
	user_event(CallId, is_a_channel(VariableName ? Term));

    otherwise |
      CallId = _,
      Term = _,
      VariableName = _.

user_event(Id, Event) :-

	self#service_id(ServiceId),
	ServiceId? = [Name | _],
	spi_utils#show_goal(Event, [], Event'),
	computation#display(term,Name#Id,[wait(Name),list,copy(Functor)]),
	wait_and_send.

  wait_and_send(Functor, Event) :-

    ground(Functor),
    known(Event) |
	computation#event(Functor(Event)).
"

	| true.
