{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module:       TestCase.Network.SIP.Parser
-- Description:  Test of SIP parser (whole messages are tested).
-- Copyright:    Copyright (c) 2015-2016 Jan Sipr
-- License:      MIT
module TestCase.Network.SIP.Parser (tests)
  where

import Control.Monad ((>>=), return)
import Data.ByteString (ByteString)
import Data.Function (($), (.), id)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Monoid ((<>))

import Test.Framework.Providers.HUnit (testCase)
import Test.Framework (Test, testGroup)
import Test.HUnit.Base ((@=?))

import Network.SIP.Parser (parseSipMessage)
import Network.SIP.Type.Message
    ( MessageType(Request, Response)
    , msgType
    , msgBody
    )
import Network.SIP.Type.RequestMethod (RequestMethod(INVITE))
import Network.SIP.Type.ResponseStatus (Status(Status), ResponseCode(OK_200))
import Network.SIP.Type.Source (mkSource)
import Network.SIP.Type.Uri (Scheme(SIP), Uri(Uri))

testMsg1 :: ByteString
testMsg1 = "INVITE sip:13@10.10.1.13 SIP/2.0\r\n"
    <> "Via: SIP/2.0/UDP 10.10.1.99:5060;branch=z9hG4bK343bf628;rport\r\n"
    <> "From: \"Test 15\" <sip:15@10.10.1.99>tag=as58f4201b\r\n"
    <> "To: <sip:13@10.10.1.13>\r\n"
    <> "Contact: <sip:15@10.10.1.99>\r\n"
    <> "Call-ID: 326371826c80e17e6cf6c29861eb2933@10.10.1.99\r\n"
    <> "CSeq: 102INVITE\r\n"
    <> "User-Agent: Asterisk PBX\r\n"
    <> "Max-Forwards: 70\r\n"
    <> "Date: Wed, 06 Dec 2009 14:12:45 GMT\r\n"
    <> "Allow: INVITE, ACK, CANCEL, OPTIONS, BYE, REFER,SUBSCRIBE, NOTIFY\r\n"
    <> "Supported: replaces\r\n"
    <> "Content-Type: application/sdp\r\n"
    <> "Content-Length: 258\r\n"
    <> "\r\n"

testMsg1Content :: ByteString
testMsg1Content = "v=0\r\n"
    <> "o=root 1821 1821 IN IP4 10.10.1.99\r\n"
    <> "s=session\r\n"
    <> "c=IN IP4 10.10.1.99\r\n"
    <> "t=0 0\r\n"
    <> "m=audio 11424 RTP/AVP 0 8 101\r\n"
    <> "a=rtpmap:0 PCMU/8000\r\n"
    <> "a=rtpmap:8 PCMA/8000\r\n"
    <> "a=rtpmap:101 telephone-event/8000\r\n"
    <> "a=fmtp:101 0-16\r\n"
    <> "a=silenceSupp:off - - - -\r\n"
    <> "a=ptime:20\r\n"
    <> "a=sendrecv\r\n"

testMsg2 :: ByteString
testMsg2 = "SIP/2.0 200 OK\r\n"
    <> "Via: SIP/2.0/UDP bobspc.biloxi.com:5060;branch=z9hG4bKnashds7\r\n"
    <> " ;received=192.0.2.4\r\n"
    <> "To: Bob <sip:bob@biloxi.com>;tag=2493k59kd\r\n"
    <> "From: Bob <sip:bob@biloxi.com>;tag=456248\r\n"
    <> "Call-ID: 843817637684230@998sdasdh09\r\n"
    <> "CSeq: 1826 REGISTER\r\n"
    <> "Contact: <sip:bob@192.0.2.4>\r\n"
    <> "Expires: 7200\r\n"
    <> "Content-Length: 0\r\n"
    <> "\r\n"

runMsgTest :: ByteString -> Maybe ByteString -> MessageType -> [Test]
runMsgTest msg content mt =
    [ testCase "Test msg type" $ getMsg >>= ((@=?) mt . msgType)
    , testCase "Test msg body" $ getMsg >>= ((@=?) content . msgBody)
    ]
  where
    getMsg = mkSource (return $ msg <> maybe "" id content) >>= parseSipMessage

tests :: [Test]
tests =
    [ testGroup "Basic header parser unit tests"
        [ testGroup "message 1"
            $ runMsgTest testMsg1 (Just testMsg1Content)
                (Request INVITE (Uri SIP (Just "13") "10.10.1.13" Nothing))
        , testGroup "message 2"
            $ runMsgTest testMsg2 Nothing
                (Response (Status OK_200 ""))
        ]
    ]
