{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module:       TestCase.Network.SIP.Serialization
-- Description:  Test of complete SIP messages serialization.
-- Copyright:    Copyright (c) 2015-2016 Jan Sipr
-- License:      MIT
module TestCase.Network.SIP.Serialization (tests)
  where

import Data.ByteString (ByteString)
import Data.Function (($))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>))

import Test.Framework.Providers.HUnit (testCase)
import Test.Framework (Test, testGroup)
import Test.HUnit.Base ((@=?), Assertion)

import Network.SIP.Serialization (serializeSipMessage)
import Network.SIP.Type.Message
    ( Message(Message)
    , MessageType(Request, Response)
    )
import Network.SIP.Type.Header
  ( HeaderName
      ( Allow
      , CallID
      , Contact
      , Contact
      , ContentLength
      , ContentType
      , CSeq
      , Date
      , From
      , MaxForwards
      , Supported
      , To
      , UserAgent
      , Via
      )
  )
import Network.SIP.Type.RequestMethod (RequestMethod(INVITE))
import Network.SIP.Type.ResponseStatus
    ( Status(Status)
    , ResponseCode(Ringing_180)
    )
import Network.SIP.Type.Uri (Scheme(SIP), Uri(Uri))

testMsg1 :: Message
testMsg1 = Message
    (Request INVITE $ Uri SIP (Just "13") "10.10.1.13" Nothing)
    [ (Via, "SIP/2.0/UDP 10.10.1.99:5060;branch=z9hG4bK343bf628;rport")
    , (From, "\"Test 15\" <sip:15@10.10.1.99>tag=as58f4201b")
    , (To, "<sip:13@10.10.1.13>")
    , (Contact, "<sip:15@10.10.1.99>")
    , (CallID, "326371826c80e17e6cf6c29861eb2933@10.10.1.99")
    , (CSeq, "102INVITE")
    , (UserAgent, "Asterisk PBX")
    , (MaxForwards, "70")
    , (Date, "Wed, 06 Dec 2009 14:12:45 GMT")
    , (Allow, "INVITE, ACK, CANCEL, OPTIONS, BYE, REFER,SUBSCRIBE, NOTIFY")
    , (Supported, "replaces")
    , (ContentType, "application/sdp")
    , (ContentLength, "258")
    ]
    (Just $ "v=0\r\no=root 1821 1821 IN IP4 10.10.1.99\r\ns=session\r\n"
        <> "c=IN IP4 10.10.1.99\r\nt=0 0\r\n"
        <> "m=audio 11424 RTP/AVP 0 8 101\r\na=rtpmap:0 PCMU/8000\r\n"
        <> "a=rtpmap:8 PCMA/8000\r\na=rtpmap:101 telephone-event/8000\r\n"
        <> "a=fmtp:101 0-16\r\na=silenceSupp:off - - - -\r\na=ptime:20"
        <> "\r\na=sendrecv\r\n"
    )

testMsg1Result :: ByteString
testMsg1Result = "INVITE sip:13@10.10.1.13 SIP/2.0\r\n"
    <> "Via: SIP/2.0/UDP 10.10.1.99:5060;branch=z9hG4bK343bf628;rport\r\n"
    <> "From: \"Test 15\" <sip:15@10.10.1.99>tag=as58f4201b\r\n"
    <> "To: <sip:13@10.10.1.13>\r\nContact: <sip:15@10.10.1.99>\r\n"
    <> "Call-ID: 326371826c80e17e6cf6c29861eb2933@10.10.1.99\r\n"
    <> "CSeq: 102INVITE\r\nUser-Agent: Asterisk PBX\r\nMax-Forwards: 70\r\n"
    <> "Date: Wed, 06 Dec 2009 14:12:45 GMT\r\n"
    <> "Allow: INVITE, ACK, CANCEL, OPTIONS, BYE, REFER,SUBSCRIBE, NOTIFY\r\n"
    <> "Supported: replaces\r\nContent-Type: application/sdp\r\n"
    <> "Content-Length: 258\r\n\r\nv=0\r\n"
    <> "o=root 1821 1821 IN IP4 10.10.1.99\r\ns=session\r\n"
    <> "c=IN IP4 10.10.1.99\r\nt=0 0\r\n"
    <> "m=audio 11424 RTP/AVP 0 8 101\r\na=rtpmap:0 PCMU/8000\r\na"
    <> "=rtpmap:8 PCMA/8000\r\na=rtpmap:101 telephone-event/8000\r\n"
    <> "a=fmtp:101 0-16\r\na=silenceSupp:off - - - -\r\n"
    <> "a=ptime:20\r\na=sendrecv\r\n"

testMsg2 :: Message
testMsg2 = Message
    (Response (Status Ringing_180 ""))
    [ (Via, "SIP/2.0/TCP client.atlanta.example.com:5060;branch=z9hG4bK74bf9"
        <> ";received=192.0.2.101")
    , (From, "Alice <sip:alice@atlanta.example.com>;tag=9fxced76sl")
    , (To, "Bob <sip:bob@biloxi.example.com>;tag=8321234356")
    , (CallID, "3848276298220188511@atlanta.example.com")
    , (CSeq, "1 INVITE")
    , (Contact, "<sip:bob@client.biloxi.example.com;transport=tcp>")
    , (ContentLength, "0")
    ]
    Nothing

testMsg2Result :: ByteString
testMsg2Result =
    "SIP/2.0 180 Ringing\r\n"
    <> "Via: SIP/2.0/TCP client.atlanta.example.com:5060;branch=z9hG4bK74bf9"
    <>  ";received=192.0.2.101\r\n"
    <> "From: Alice <sip:alice@atlanta.example.com>;tag=9fxced76sl\r\n"
    <> "To: Bob <sip:bob@biloxi.example.com>;tag=8321234356\r\n"
    <> "Call-ID: 3848276298220188511@atlanta.example.com\r\n"
    <> "CSeq: 1 INVITE\r\n"
    <> "Contact: <sip:bob@client.biloxi.example.com;transport=tcp>\r\n"
    <> "Content-Length: 0\r\n\r\n"

runMsgTest :: Message -> ByteString -> Assertion
runMsgTest msg res = res @=? serializeSipMessage msg

tests :: [Test]
tests =
    [ testGroup "Seriali SIP message"
        [ testCase "message 1"
            $ runMsgTest testMsg1 testMsg1Result
        , testCase "message 2"
            $ runMsgTest testMsg2 testMsg2Result
        ]
    ]
