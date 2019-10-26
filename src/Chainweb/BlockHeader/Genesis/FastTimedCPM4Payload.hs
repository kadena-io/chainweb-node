{-# LANGUAGE QuasiQuotes #-}

-- This module is auto-generated. DO NOT EDIT IT MANUALLY.

module Chainweb.BlockHeader.Genesis.FastTimedCPM4Payload ( payloadBlock ) where

import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (decodeThrow)

import NeatInterpolation (text)

import Chainweb.Payload (PayloadWithOutputs)
import Chainweb.Utils (fromJuste)

payloadBlock :: PayloadWithOutputs
payloadBlock = fromJuste $ decodeThrow $ encodeUtf8 [text|
transactions:
- - eyJoYXNoIjoiNDhUMExqQW5TRnBGV3h2dmFQVi1fNkUtQ2pEQVBoV1lVRldidnlmMmxGcyIsInNpZ3MiOltdLCJjbWQiOiJ7XCJuZXR3b3JrSWRcIjpudWxsLFwicGF5bG9hZFwiOntcImV4ZWNcIjp7XCJkYXRhXCI6bnVsbCxcImNvZGVcIjpcIihpbnRlcmZhY2UgZnVuZ2libGUtdjFcXG5cXG4gIFxcXCIgU3RhbmRhcmQgZm9yIGZ1bmdpYmxlIGNvaW5zIGFuZCB0b2tlbnMgYXMgc3BlY2lmaWVkIGluIEtJUC0wMDAyLiBcXFwiXFxuXFxuICAgOyAtLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tXFxuICAgOyBTY2hlbWFcXG5cXG4gICAoZGVmc2NoZW1hIGFjY291bnQtZGV0YWlsc1xcbiAgICBAZG9jIFxcXCJTY2hlbWEgZm9yIHJlc3VsdHMgb2YgJ2FjY291bnQnIG9wZXJhdGlvbi5cXFwiXFxuICAgIEBtb2RlbCBbIChpbnZhcmlhbnQgKCE9IFxcXCJcXFwiIHNlbmRlcikpIF1cXG5cXG4gICAgYWNjb3VudDpzdHJpbmdcXG4gICAgYmFsYW5jZTpkZWNpbWFsXFxuICAgIGd1YXJkOmd1YXJkKVxcblxcblxcbiAgIDsgLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLVxcbiAgIDsgQ2Fwc1xcblxcbiAgIChkZWZjYXAgVFJBTlNGRVI6Ym9vbFxcbiAgICAgKCBzZW5kZXI6c3RyaW5nXFxuICAgICAgIHJlY2VpdmVyOnN0cmluZ1xcbiAgICAgICBhbW91bnQ6ZGVjaW1hbFxcbiAgICAgKVxcbiAgICAgQGRvYyBcXFwiIE1hbmFnZWQgY2FwYWJpbGl0eSBzZWFsaW5nIEFNT1VOVCBmb3IgdHJhbnNmZXIgZnJvbSBTRU5ERVIgdG8gXFxcXFxcbiAgICAgICAgICBcXFxcIFJFQ0VJVkVSLiBQZXJtaXRzIGFueSBudW1iZXIgb2YgdHJhbnNmZXJzIHVwIHRvIEFNT1VOVC5cXFwiXFxuICAgICBAbWFuYWdlZCBhbW91bnQgVFJBTlNGRVItbWdyXFxuICAgICApXFxuXFxuICAgKGRlZnVuIFRSQU5TRkVSLW1ncjpkZWNpbWFsXFxuICAgICAoIG1hbmFnZWQ6ZGVjaW1hbFxcbiAgICAgICByZXF1ZXN0ZWQ6ZGVjaW1hbFxcbiAgICAgKVxcbiAgICAgQGRvYyBcXFwiIE1hbmFnZXMgVFJBTlNGRVIgQU1PVU5UIGxpbmVhcmx5LCBcXFxcXFxuICAgICAgICAgIFxcXFwgc3VjaCB0aGF0IGEgcmVxdWVzdCBmb3IgMS4wIGFtb3VudCBvbiBhIDMuMCBcXFxcXFxuICAgICAgICAgIFxcXFwgbWFuYWdlZCBxdWFudGl0eSBlbWl0cyB1cGRhdGVkIGFtb3VudCAyLjAuXFxcIlxcbiAgICAgKVxcblxcbiAgIDsgLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLVxcbiAgIDsgRnVuY3Rpb25hbGl0eVxcblxcbiAgIChkZWZ1biB0cmFuc2Zlci1jcmVhdGU6c3RyaW5nXFxuICAgICAoIHNlbmRlcjpzdHJpbmdcXG4gICAgICAgcmVjZWl2ZXI6c3RyaW5nXFxuICAgICAgIHJlY2VpdmVyLWd1YXJkOmd1YXJkXFxuICAgICAgIGFtb3VudDpkZWNpbWFsXFxuICAgICApXFxuICAgICBAZG9jIFxcXCIgVHJhbnNmZXIgQU1PVU5UIGJldHdlZW4gYWNjb3VudHMgU0VOREVSIGFuZCBSRUNFSVZFUi4gXFxcXFxcbiAgICAgICAgICBcXFxcIEZhaWxzIGlmIFNFTkRFUiBkb2VzIG5vdCBleGlzdC4gSWYgUkVDRUlWRVIgZXhpc3RzLCBndWFyZCBcXFxcXFxuICAgICAgICAgIFxcXFwgbXVzdCBtYXRjaCBleGlzdGluZyB2YWx1ZS4gSWYgUkVDRUlWRVIgZG9lcyBub3QgZXhpc3QsIFxcXFxcXG4gICAgICAgICAgXFxcXCBSRUNFSVZFUiBhY2NvdW50IGlzIGNyZWF0ZWQgdXNpbmcgUkVDRUlWRVItR1VBUkQuIFxcXFxcXG4gICAgICAgICAgXFxcXCBTdWJqZWN0IHRvIG1hbmFnZW1lbnQgYnkgVFJBTlNGRVIgY2FwYWJpbGl0eS5cXFwiXFxuICAgICBAbW9kZWwgWyAocHJvcGVydHkgKD4gYW1vdW50IDAuMCkpXFxuICAgICAgICAgICAgICAocHJvcGVydHkgKCE9IHNlbmRlciBcXFwiXFxcIikpXFxuICAgICAgICAgICAgICAocHJvcGVydHkgKCE9IHJlY2VpdmVyIFxcXCJcXFwiKSlcXG4gICAgICAgICAgICAgIChwcm9wZXJ0eSAoIT0gc2VuZGVyIHJlY2VpdmVyKSlcXG4gICAgICAgICAgICBdXFxuICAgICApXFxuXFxuICAgKGRlZnBhY3QgdHJhbnNmZXItY3Jvc3NjaGFpbjpzdHJpbmdcXG4gICAgICggc2VuZGVyOnN0cmluZ1xcbiAgICAgICByZWNlaXZlcjpzdHJpbmdcXG4gICAgICAgcmVjZWl2ZXItZ3VhcmQ6Z3VhcmRcXG4gICAgICAgdGFyZ2V0LWNoYWluOnN0cmluZ1xcbiAgICAgICBhbW91bnQ6ZGVjaW1hbFxcbiAgICAgKVxcbiAgICAgQGRvYyBcXFwiIDItc3RlcCBwYWN0IHRvIHRyYW5zZmVyIEFNT1VOVCBmcm9tIFNFTkRFUiBvbiBjdXJyZW50IGNoYWluIFxcXFxcXG4gICAgICAgICAgXFxcXCB0byBSRUNFSVZFUiBvbiBUQVJHRVQtQ0hBSU4gdmlhIFNQViBwcm9vZi4gXFxcXFxcbiAgICAgICAgICBcXFxcIFRBUkdFVC1DSEFJTiBtdXN0IGJlIGRpZmZlcmVudCB0aGFuIGN1cnJlbnQgY2hhaW4gaWQuIFxcXFxcXG4gICAgICAgICAgXFxcXCBGaXJzdCBzdGVwIGRlYml0cyBBTU9VTlQgY29pbnMgaW4gU0VOREVSIGFjY291bnQgYW5kIHlpZWxkcyBcXFxcXFxuICAgICAgICAgIFxcXFwgUkVDRUlWRVIsIFJFQ0VJVkVSX0dVQVJEIGFuZCBBTU9VTlQgdG8gVEFSR0VULUNIQUlOLiBcXFxcXFxuICAgICAgICAgIFxcXFwgU2Vjb25kIHN0ZXAgY29udGludWF0aW9uIGlzIHNlbnQgaW50byBUQVJHRVQtQ0hBSU4gd2l0aCBwcm9vZiBcXFxcXFxuICAgICAgICAgIFxcXFwgb2J0YWluZWQgZnJvbSB0aGUgc3B2ICdvdXRwdXQnIGVuZHBvaW50IG9mIENoYWlud2ViLiBcXFxcXFxuICAgICAgICAgIFxcXFwgUHJvb2YgaXMgdmFsaWRhdGVkIGFuZCBSRUNFSVZFUiBpcyBjcmVkaXRlZCB3aXRoIEFNT1VOVCBcXFxcXFxuICAgICAgICAgIFxcXFwgY3JlYXRpbmcgYWNjb3VudCB3aXRoIFJFQ0VJVkVSX0dVQVJEIGFzIG5lY2Vzc2FyeS5cXFwiXFxuICAgICBAbW9kZWwgWyAocHJvcGVydHkgKD4gYW1vdW50IDAuMCkpXFxuICAgICAgICAgICAgICAocHJvcGVydHkgKCE9IHNlbmRlciBcXFwiXFxcIikpXFxuICAgICAgICAgICAgICAocHJvcGVydHkgKCE9IHJlY2VpdmVyIFxcXCJcXFwiKSlcXG4gICAgICAgICAgICAgIChwcm9wZXJ0eSAoIT0gc2VuZGVyIHJlY2VpdmVyKSlcXG4gICAgICAgICAgICAgIChwcm9wZXJ0eSAoIT0gdGFyZ2V0LWNoYWluIFxcXCJcXFwiKSlcXG4gICAgICAgICAgICBdXFxuICAgICApXFxuXFxuICAgKGRlZnVuIGdldC1iYWxhbmNlOmRlY2ltYWxcXG4gICAgICggYWNjb3VudDpzdHJpbmcgKVxcbiAgICAgXFxcIiBHZXQgYmFsYW5jZSBmb3IgQUNDT1VOVC4gRmFpbHMgaWYgYWNjb3VudCBkb2VzIG5vdCBleGlzdC5cXFwiXFxuICAgICApXFxuXFxuICAgKGRlZnVuIGRldGFpbHM6b2JqZWN0e2FjY291bnQtZGV0YWlsc31cXG4gICAgICggYWNjb3VudDogc3RyaW5nIClcXG4gICAgIFxcXCIgR2V0IGFuIG9iamVjdCB3aXRoIGRldGFpbHMgb2YgQUNDT1VOVC4gXFxcXFxcbiAgICAgXFxcXCBGYWlscyBpZiBhY2NvdW50IGRvZXMgbm90IGV4aXN0LlxcXCJcXG4gICAgIClcXG5cXG4gICAoZGVmdW4gcHJlY2lzaW9uOmludGVnZXJcXG4gICAgICgpXFxuICAgICBcXFwiUmV0dXJuIHRoZSBtYXhpbXVtIGFsbG93ZWQgZGVjaW1hbCBwcmVjaXNpb24uXFxcIlxcbiAgICAgKVxcblxcbiAgIChkZWZ1biBlbmZvcmNlLXVuaXQ6Ym9vbFxcbiAgICAgKCBhbW91bnQ6ZGVjaW1hbCApXFxuICAgICBcXFwiIEVuZm9yY2UgbWluaW11bSBwcmVjaXNpb24gYWxsb3dlZCBmb3IgdHJhbnNhY3Rpb25zLlxcXCJcXG4gICAgIClcXG5cXG4gICAoZGVmdW4gY3JlYXRlLWFjY291bnQ6c3RyaW5nXFxuICAgICAoIGFjY291bnQ6c3RyaW5nXFxuICAgICAgIGd1YXJkOmd1YXJkXFxuICAgICApXFxuICAgICBcXFwiIENyZWF0ZSBBQ0NPVU5UIHdpdGggMC4wIGJhbGFuY2UsIHdpdGggR1VBUkQgY29udHJvbGxpbmcgYWNjZXNzLlxcXCJcXG4gICAgIClcXG5cXG4gICAoZGVmdW4gcm90YXRlOnN0cmluZ1xcbiAgICAgKCBhY2NvdW50OnN0cmluZ1xcbiAgICAgICBuZXctZ3VhcmQ6Z3VhcmRcXG4gICAgIClcXG4gICAgIFxcXCIgUm90YXRlIGd1YXJkIGZvciBBQ0NPVU5ULiBUcmFuc2FjdGlvbiBpcyB2YWxpZGF0ZWQgYWdhaW5zdCBcXFxcXFxuICAgICBcXFxcIGV4aXN0aW5nIGd1YXJkIGJlZm9yZSBpbnN0YWxsaW5nIG5ldyBndWFyZC4gXFxcIlxcbiAgICAgKVxcblxcbilcXG5cIn19LFwic2lnbmVyc1wiOltdLFwibWV0YVwiOntcImNyZWF0aW9uVGltZVwiOjAsXCJ0dGxcIjoxNzI4MDAsXCJnYXNMaW1pdFwiOjAsXCJjaGFpbklkXCI6XCJcIixcImdhc1ByaWNlXCI6MCxcInNlbmRlclwiOlwiXCJ9LFwibm9uY2VcIjpcImdlbmVzaXMtMDFcIn0ifQ
  - eyJnYXMiOjAsInJlc3VsdCI6eyJzdGF0dXMiOiJzdWNjZXNzIiwiZGF0YSI6IkxvYWRlZCBpbnRlcmZhY2UgZnVuZ2libGUtdjEifSwicmVxS2V5IjoiNDhUMExqQW5TRnBGV3h2dmFQVi1fNkUtQ2pEQVBoV1lVRldidnlmMmxGcyIsImxvZ3MiOiJRRmEyOHRuOXkydFdMVzdUc3lHdzNOTkhpYzhxYjJ6UGtudXRpWWhnQXc4IiwibWV0YURhdGEiOm51bGwsImNvbnRpbnVhdGlvbiI6bnVsbCwidHhJZCI6MH0
- - eyJoYXNoIjoiWEdQRVFEazVQSXZRa3BxMEdHa2dOVG1vLW1qa2k2M1pQZ0VSX2tvdnhxNCIsInNpZ3MiOltdLCJjbWQiOiJ7XCJuZXR3b3JrSWRcIjpudWxsLFwicGF5bG9hZFwiOntcImV4ZWNcIjp7XCJkYXRhXCI6bnVsbCxcImNvZGVcIjpcIihtb2R1bGUgY29pbiBHT1ZFUk5BTkNFXFxuXFxuICBAZG9jIFxcXCInY29pbicgcmVwcmVzZW50cyB0aGUgS2FkZW5hIENvaW4gQ29udHJhY3QuIFRoaXMgY29udHJhY3QgcHJvdmlkZXMgYm90aCB0aGUgXFxcXFxcbiAgXFxcXGJ1eS9yZWRlZW0gZ2FzIHN1cHBvcnQgaW4gdGhlIGZvcm0gb2YgJ2Z1bmQtdHgnLCBhcyB3ZWxsIGFzIHRyYW5zZmVyLCAgICAgICBcXFxcXFxuICBcXFxcY3JlZGl0LCBkZWJpdCwgY29pbmJhc2UsIGFjY291bnQgY3JlYXRpb24gYW5kIHF1ZXJ5LCBhcyB3ZWxsIGFzIFNQViBidXJuICAgIFxcXFxcXG4gIFxcXFxjcmVhdGUuIFRvIGFjY2VzcyB0aGUgY29pbiBjb250cmFjdCwgeW91IG1heSB1c2UgaXRzIGZ1bGx5LXF1YWxpZmllZCBuYW1lLCAgXFxcXFxcbiAgXFxcXG9yIGlzc3VlIHRoZSAnKHVzZSBjb2luKScgY29tbWFuZCBpbiB0aGUgYm9keSBvZiBhIG1vZHVsZSBkZWNsYXJhdGlvbi5cXFwiXFxuXFxuICBAbW9kZWxcXG4gICAgWyAoZGVmcHJvcGVydHkgY29uc2VydmVzLW1hc3NcXG4gICAgICAgICg9IChjb2x1bW4tZGVsdGEgY29pbi10YWJsZSAnYmFsYW5jZSkgMC4wKSlcXG5cXG4gICAgICAoZGVmcHJvcGVydHkgdmFsaWQtYWNjb3VudCAoYWNjb3VudDpzdHJpbmcpXFxuICAgICAgICAoYW5kXFxuICAgICAgICAgICg-PSAobGVuZ3RoIGFjY291bnQpIDMpXFxuICAgICAgICAgICg8PSAobGVuZ3RoIGFjY291bnQpIDI1NikpKVxcbiAgICBdXFxuXFxuICAoaW1wbGVtZW50cyBmdW5naWJsZS12MSlcXG5cXG4gIDsgLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS1cXG4gIDsgU2NoZW1hcyBhbmQgVGFibGVzXFxuXFxuICAoZGVmc2NoZW1hIGNvaW4tc2NoZW1hXFxuICAgIEBkb2MgXFxcIlRoZSBjb2luIGNvbnRyYWN0IHRva2VuIHNjaGVtYVxcXCJcXG4gICAgQG1vZGVsIFsgKGludmFyaWFudCAoPj0gYmFsYW5jZSAwLjApKSBdXFxuXFxuICAgIGJhbGFuY2U6ZGVjaW1hbFxcbiAgICBndWFyZDpndWFyZClcXG5cXG4gIChkZWZ0YWJsZSBjb2luLXRhYmxlOntjb2luLXNjaGVtYX0pXFxuXFxuICA7IC0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tXFxuICA7IENhcGFiaWxpdGllc1xcblxcbiAgKGRlZmNhcCBHT1ZFUk5BTkNFICgpXFxuICAgIChlbmZvcmNlIGZhbHNlIFxcXCJFbmZvcmNlIG5vbi11cGdyYWRlYWJpbGl0eVxcXCIpKVxcblxcbiAgKGRlZmNhcCBHQVMgKClcXG4gICAgXFxcIk1hZ2ljIGNhcGFiaWxpdHkgdG8gcHJvdGVjdCBnYXMgYnV5IGFuZCByZWRlZW1cXFwiXFxuICAgIHRydWUpXFxuXFxuICAoZGVmY2FwIENPSU5CQVNFICgpXFxuICAgIFxcXCJNYWdpYyBjYXBhYmlsaXR5IHRvIHByb3RlY3QgbWluZXIgcmV3YXJkXFxcIlxcbiAgICB0cnVlKVxcblxcbiAgKGRlZmNhcCBHRU5FU0lTICgpXFxuICAgIFxcXCJNYWdpYyBjYXBhYmlsaXR5IGNvbnN0cmFpbmluZyBnZW5lc2lzIHRyYW5zYWN0aW9uc1xcXCJcXG4gICAgdHJ1ZSlcXG5cXG4gIChkZWZjYXAgREVCSVQgKHNlbmRlcjpzdHJpbmcpXFxuICAgIFxcXCJDYXBhYmlsaXR5IGZvciBtYW5hZ2luZyBkZWJpdGluZyBvcGVyYXRpb25zXFxcIlxcbiAgICAoZW5mb3JjZS1ndWFyZCAoYXQgJ2d1YXJkIChyZWFkIGNvaW4tdGFibGUgc2VuZGVyKSkpXFxuICAgIChlbmZvcmNlICghPSBzZW5kZXIgXFxcIlxcXCIpIFxcXCJ2YWxpZCBzZW5kZXJcXFwiKSlcXG5cXG4gIChkZWZjYXAgQ1JFRElUIChyZWNlaXZlcjpzdHJpbmcpXFxuICAgIFxcXCJDYXBhYmlsaXR5IGZvciBtYW5hZ2luZyBjcmVkaXRpbmcgb3BlcmF0aW9uc1xcXCJcXG4gICAgKGVuZm9yY2UgKCE9IHJlY2VpdmVyIFxcXCJcXFwiKSBcXFwidmFsaWQgcmVjZWl2ZXJcXFwiKSlcXG5cXG4gIChkZWZjYXAgVFJBTlNGRVI6Ym9vbFxcbiAgICAoIHNlbmRlcjpzdHJpbmdcXG4gICAgICByZWNlaXZlcjpzdHJpbmdcXG4gICAgICBhbW91bnQ6ZGVjaW1hbFxcbiAgICApXFxuICAgIEBtYW5hZ2VkIGFtb3VudCBUUkFOU0ZFUi1tZ3JcXG4gICAgKGVuZm9yY2UgKCE9IHNlbmRlciByZWNlaXZlcikgXFxcInNhbWUgc2VuZGVyIGFuZCByZWNlaXZlclxcXCIpXFxuICAgIChlbmZvcmNlLXVuaXQgYW1vdW50KVxcbiAgICAoZW5mb3JjZSAoPiBhbW91bnQgMC4wKSBcXFwiUG9zaXRpdmUgYW1vdW50XFxcIilcXG4gICAgKGNvbXBvc2UtY2FwYWJpbGl0eSAoREVCSVQgc2VuZGVyKSlcXG4gICAgKGNvbXBvc2UtY2FwYWJpbGl0eSAoQ1JFRElUIHJlY2VpdmVyKSlcXG4gIClcXG5cXG4gIChkZWZ1biBUUkFOU0ZFUi1tZ3I6ZGVjaW1hbFxcbiAgICAoIG1hbmFnZWQ6ZGVjaW1hbFxcbiAgICAgIHJlcXVlc3RlZDpkZWNpbWFsXFxuICAgIClcXG5cXG4gICAgKGxldCAoKG5ld2JhbCAoLSBtYW5hZ2VkIHJlcXVlc3RlZCkpKVxcbiAgICAgIChlbmZvcmNlICg-PSBuZXdiYWwgMC4wKVxcbiAgICAgICAgKGZvcm1hdCBcXFwiVFJBTlNGRVIgZXhjZWVkZWQgZm9yIGJhbGFuY2Uge31cXFwiIFttYW5hZ2VkXSkpXFxuICAgICAgbmV3YmFsKVxcbiAgKVxcblxcbiAgOyAtLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLVxcbiAgOyBDb25zdGFudHNcXG5cXG4gIChkZWZjb25zdCBDT0lOX0NIQVJTRVQgQ0hBUlNFVF9MQVRJTjFcXG4gICAgXFxcIlRoZSBkZWZhdWx0IGNvaW4gY29udHJhY3QgY2hhcmFjdGVyIHNldFxcXCIpXFxuXFxuICAoZGVmY29uc3QgTUlOSU1VTV9QUkVDSVNJT04gMTJcXG4gICAgXFxcIk1pbmltdW0gYWxsb3dlZCBwcmVjaXNpb24gZm9yIGNvaW4gdHJhbnNhY3Rpb25zXFxcIilcXG5cXG4gIChkZWZjb25zdCBNSU5JTVVNX0FDQ09VTlRfTEVOR1RIIDNcXG4gICAgXFxcIk1pbmltdW0gYWNjb3VudCBsZW5ndGggYWRtaXNzaWJsZSBmb3IgY29pbiBhY2NvdW50c1xcXCIpXFxuXFxuICAoZGVmY29uc3QgTUFYSU1VTV9BQ0NPVU5UX0xFTkdUSCAyNTZcXG4gICAgXFxcIk1heGltdW0gYWNjb3VudCBuYW1lIGxlbmd0aCBhZG1pc3NpYmxlIGZvciBjb2luIGFjY291bnRzXFxcIilcXG5cXG4gIDsgLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS1cXG4gIDsgVXRpbGl0aWVzXFxuXFxuICAoZGVmdW4gZW5mb3JjZS11bml0OmJvb2wgKGFtb3VudDpkZWNpbWFsKVxcbiAgICBAZG9jIFxcXCJFbmZvcmNlIG1pbmltdW0gcHJlY2lzaW9uIGFsbG93ZWQgZm9yIGNvaW4gdHJhbnNhY3Rpb25zXFxcIlxcblxcbiAgICAoZW5mb3JjZVxcbiAgICAgICg9IChmbG9vciBhbW91bnQgTUlOSU1VTV9QUkVDSVNJT04pXFxuICAgICAgICAgYW1vdW50KVxcbiAgICAgIChmb3JtYXQgXFxcIkFtb3VudCB2aW9sYXRlcyBtaW5pbXVtIHByZWNpc2lvbjoge31cXFwiIFthbW91bnRdKSlcXG4gICAgKVxcblxcbiAgKGRlZnVuIHZhbGlkYXRlLWFjY291bnQgKGFjY291bnQ6c3RyaW5nKVxcbiAgICBAZG9jIFxcXCJFbmZvcmNlIHRoYXQgYW4gYWNjb3VudCBuYW1lIGNvbmZvcm1zIHRvIHRoZSBjb2luIGNvbnRyYWN0IFxcXFxcXG4gICAgICAgICBcXFxcbWluaW11bSBhbmQgbWF4aW11bSBsZW5ndGggcmVxdWlyZW1lbnRzLCBhcyB3ZWxsIGFzIHRoZSAgICBcXFxcXFxuICAgICAgICAgXFxcXGxhdGluLTEgY2hhcmFjdGVyIHNldC5cXFwiXFxuXFxuICAgIChlbmZvcmNlXFxuICAgICAgKGlzLWNoYXJzZXQgQ09JTl9DSEFSU0VUIGFjY291bnQpXFxuICAgICAgKGZvcm1hdFxcbiAgICAgICAgXFxcIkFjY291bnQgZG9lcyBub3QgY29uZm9ybSB0byB0aGUgY29pbiBjb250cmFjdCBjaGFyc2V0OiB7fVxcXCJcXG4gICAgICAgIFthY2NvdW50XSkpXFxuXFxuICAgIChsZXQgKChhY2NvdW50LWxlbmd0aCAobGVuZ3RoIGFjY291bnQpKSlcXG5cXG4gICAgICAoZW5mb3JjZVxcbiAgICAgICAgKD49IGFjY291bnQtbGVuZ3RoIE1JTklNVU1fQUNDT1VOVF9MRU5HVEgpXFxuICAgICAgICAoZm9ybWF0XFxuICAgICAgICAgIFxcXCJBY2NvdW50IG5hbWUgZG9lcyBub3QgY29uZm9ybSB0byB0aGUgbWluIGxlbmd0aCByZXF1aXJlbWVudDoge31cXFwiXFxuICAgICAgICAgIFthY2NvdW50XSkpXFxuXFxuICAgICAgKGVuZm9yY2VcXG4gICAgICAgICg8PSBhY2NvdW50LWxlbmd0aCBNQVhJTVVNX0FDQ09VTlRfTEVOR1RIKVxcbiAgICAgICAgKGZvcm1hdFxcbiAgICAgICAgICBcXFwiQWNjb3VudCBuYW1lIGRvZXMgbm90IGNvbmZvcm0gdG8gdGhlIG1heCBsZW5ndGggcmVxdWlyZW1lbnQ6IHt9XFxcIlxcbiAgICAgICAgICBbYWNjb3VudF0pKVxcbiAgICAgIClcXG4gIClcXG5cXG4gIDsgLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS1cXG4gIDsgQ29pbiBDb250cmFjdFxcblxcbiAgKGRlZnVuIGdhcy1vbmx5ICgpXFxuICAgIFxcXCJQcmVkaWNhdGUgZm9yIGdhcy1vbmx5IHVzZXIgZ3VhcmRzLlxcXCJcXG4gICAgKHJlcXVpcmUtY2FwYWJpbGl0eSAoR0FTKSkpXFxuXFxuICAoZGVmdW4gZ2FzLWd1YXJkIChndWFyZDpndWFyZClcXG4gICAgXFxcIlByZWRpY2F0ZSBmb3IgZ2FzICsgc2luZ2xlIGtleSB1c2VyIGd1YXJkc1xcXCJcXG4gICAgKGVuZm9yY2Utb25lXFxuICAgICAgXFxcIkVuZm9yY2UgZWl0aGVyIHRoZSBwcmVzZW5jZSBvZiBhIEdBUyBjYXAgb3Iga2V5c2V0XFxcIlxcbiAgICAgIFsgKGdhcy1vbmx5KVxcbiAgICAgICAgKGVuZm9yY2UtZ3VhcmQgZ3VhcmQpXFxuICAgICAgXSkpXFxuXFxuICAoZGVmdW4gYnV5LWdhczpzdHJpbmcgKHNlbmRlcjpzdHJpbmcgdG90YWw6ZGVjaW1hbClcXG4gICAgQGRvYyBcXFwiVGhpcyBmdW5jdGlvbiBkZXNjcmliZXMgdGhlIG1haW4gJ2dhcyBidXknIG9wZXJhdGlvbi4gQXQgdGhpcyBwb2ludCBcXFxcXFxuICAgIFxcXFxNSU5FUiBoYXMgYmVlbiBjaG9zZW4gZnJvbSB0aGUgcG9vbCwgYW5kIHdpbGwgYmUgdmFsaWRhdGVkLiBUaGUgU0VOREVSICAgXFxcXFxcbiAgICBcXFxcb2YgdGhpcyB0cmFuc2FjdGlvbiBoYXMgc3BlY2lmaWVkIGEgZ2FzIGxpbWl0IExJTUlUIChtYXhpbXVtIGdhcykgZm9yICAgIFxcXFxcXG4gICAgXFxcXHRoZSB0cmFuc2FjdGlvbiwgYW5kIHRoZSBwcmljZSBpcyB0aGUgc3BvdCBwcmljZSBvZiBnYXMgYXQgdGhhdCB0aW1lLiAgICBcXFxcXFxuICAgIFxcXFxUaGUgZ2FzIGJ1eSB3aWxsIGJlIGV4ZWN1dGVkIHByaW9yIHRvIGV4ZWN1dGluZyBTRU5ERVIncyBjb2RlLlxcXCJcXG5cXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5ICg-IHRvdGFsIDAuMCkpXFxuICAgICAgICAgICAgIChwcm9wZXJ0eSAodmFsaWQtYWNjb3VudCBzZW5kZXIpKVxcbiAgICAgICAgICAgXVxcblxcbiAgICAodmFsaWRhdGUtYWNjb3VudCBzZW5kZXIpXFxuXFxuICAgIChlbmZvcmNlLXVuaXQgdG90YWwpXFxuICAgIChlbmZvcmNlICg-IHRvdGFsIDAuMCkgXFxcImdhcyBzdXBwbHkgbXVzdCBiZSBhIHBvc2l0aXZlIHF1YW50aXR5XFxcIilcXG5cXG4gICAgKHJlcXVpcmUtY2FwYWJpbGl0eSAoR0FTKSlcXG4gICAgKHdpdGgtY2FwYWJpbGl0eSAoREVCSVQgc2VuZGVyKVxcbiAgICAgIChkZWJpdCBzZW5kZXIgdG90YWwpKVxcbiAgICApXFxuXFxuICAoZGVmdW4gcmVkZWVtLWdhczpzdHJpbmcgKG1pbmVyOnN0cmluZyBtaW5lci1ndWFyZDpndWFyZCBzZW5kZXI6c3RyaW5nIHRvdGFsOmRlY2ltYWwpXFxuICAgIEBkb2MgXFxcIlRoaXMgZnVuY3Rpb24gZGVzY3JpYmVzIHRoZSBtYWluICdyZWRlZW0gZ2FzJyBvcGVyYXRpb24uIEF0IHRoaXMgICAgXFxcXFxcbiAgICBcXFxccG9pbnQsIHRoZSBTRU5ERVIncyB0cmFuc2FjdGlvbiBoYXMgYmVlbiBleGVjdXRlZCwgYW5kIHRoZSBnYXMgdGhhdCAgICAgIFxcXFxcXG4gICAgXFxcXHdhcyBjaGFyZ2VkIGhhcyBiZWVuIGNhbGN1bGF0ZWQuIE1JTkVSIHdpbGwgYmUgY3JlZGl0ZWQgdGhlIGdhcyBjb3N0LCAgICBcXFxcXFxuICAgIFxcXFxhbmQgU0VOREVSIHdpbGwgcmVjZWl2ZSB0aGUgcmVtYWluZGVyIHVwIHRvIHRoZSBsaW1pdFxcXCJcXG5cXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5ICg-IHRvdGFsIDAuMCkpXFxuICAgICAgICAgICAgIChwcm9wZXJ0eSAodmFsaWQtYWNjb3VudCBzZW5kZXIpKVxcbiAgICAgICAgICAgICAocHJvcGVydHkgKHZhbGlkLWFjY291bnQgbWluZXIpKVxcbiAgICAgICAgICAgXVxcblxcbiAgICAodmFsaWRhdGUtYWNjb3VudCBzZW5kZXIpXFxuICAgICh2YWxpZGF0ZS1hY2NvdW50IG1pbmVyKVxcbiAgICAoZW5mb3JjZS11bml0IHRvdGFsKVxcblxcbiAgICAocmVxdWlyZS1jYXBhYmlsaXR5IChHQVMpKVxcbiAgICAobGV0KlxcbiAgICAgICgoZmVlIChyZWFkLWRlY2ltYWwgXFxcImZlZVxcXCIpKVxcbiAgICAgICAocmVmdW5kICgtIHRvdGFsIGZlZSkpKVxcblxcbiAgICAgIChlbmZvcmNlLXVuaXQgZmVlKVxcbiAgICAgIChlbmZvcmNlICg-PSBmZWUgMC4wKVxcbiAgICAgICAgXFxcImZlZSBtdXN0IGJlIGEgbm9uLW5lZ2F0aXZlIHF1YW50aXR5XFxcIilcXG5cXG4gICAgICAoZW5mb3JjZSAoPj0gcmVmdW5kIDAuMClcXG4gICAgICAgIFxcXCJyZWZ1bmQgbXVzdCBiZSBhIG5vbi1uZWdhdGl2ZSBxdWFudGl0eVxcXCIpXFxuXFxuICAgICAgICA7IGRpcmVjdGx5IHVwZGF0ZSBpbnN0ZWFkIG9mIGNyZWRpdFxcbiAgICAgICh3aXRoLWNhcGFiaWxpdHkgKENSRURJVCBzZW5kZXIpXFxuICAgICAgICAoaWYgKD4gcmVmdW5kIDAuMClcXG4gICAgICAgICAgKHdpdGgtcmVhZCBjb2luLXRhYmxlIHNlbmRlclxcbiAgICAgICAgICAgIHsgXFxcImJhbGFuY2VcXFwiIDo9IGJhbGFuY2UgfVxcbiAgICAgICAgICAgICh1cGRhdGUgY29pbi10YWJsZSBzZW5kZXJcXG4gICAgICAgICAgICAgIHsgXFxcImJhbGFuY2VcXFwiOiAoKyBiYWxhbmNlIHJlZnVuZCkgfSkpXFxuXFxuICAgICAgICAgIFxcXCJub29wXFxcIikpXFxuXFxuICAgICAgKHdpdGgtY2FwYWJpbGl0eSAoQ1JFRElUIG1pbmVyKVxcbiAgICAgICAgKGlmICg-IGZlZSAwLjApXFxuICAgICAgICAgIChjcmVkaXQgbWluZXIgbWluZXItZ3VhcmQgZmVlKVxcbiAgICAgICAgICBcXFwibm9vcFxcXCIpKVxcbiAgICAgIClcXG5cXG4gICAgKVxcblxcbiAgKGRlZnVuIGNyZWF0ZS1hY2NvdW50OnN0cmluZyAoYWNjb3VudDpzdHJpbmcgZ3VhcmQ6Z3VhcmQpXFxuICAgIEBtb2RlbCBbIChwcm9wZXJ0eSAodmFsaWQtYWNjb3VudCBhY2NvdW50KSkgXVxcblxcbiAgICAodmFsaWRhdGUtYWNjb3VudCBhY2NvdW50KVxcblxcbiAgICAoaW5zZXJ0IGNvaW4tdGFibGUgYWNjb3VudFxcbiAgICAgIHsgXFxcImJhbGFuY2VcXFwiIDogMC4wXFxuICAgICAgLCBcXFwiZ3VhcmRcXFwiICAgOiBndWFyZFxcbiAgICAgIH0pXFxuICAgIClcXG5cXG4gIChkZWZ1biBnZXQtYmFsYW5jZTpkZWNpbWFsIChhY2NvdW50OnN0cmluZylcXG4gICAgKHdpdGgtcmVhZCBjb2luLXRhYmxlIGFjY291bnRcXG4gICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6PSBiYWxhbmNlIH1cXG4gICAgICBiYWxhbmNlXFxuICAgICAgKVxcbiAgICApXFxuXFxuICAoZGVmdW4gZGV0YWlsczpvYmplY3R7ZnVuZ2libGUtdjEuYWNjb3VudC1kZXRhaWxzfVxcbiAgICAoIGFjY291bnQ6c3RyaW5nIClcXG4gICAgKHdpdGgtcmVhZCBjb2luLXRhYmxlIGFjY291bnRcXG4gICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6PSBiYWxcXG4gICAgICAsIFxcXCJndWFyZFxcXCIgOj0gZyB9XFxuICAgICAgeyBcXFwiYWNjb3VudFxcXCIgOiBhY2NvdW50XFxuICAgICAgLCBcXFwiYmFsYW5jZVxcXCIgOiBiYWxcXG4gICAgICAsIFxcXCJndWFyZFxcXCI6IGcgfSlcXG4gICAgKVxcblxcbiAgKGRlZnVuIHJvdGF0ZTpzdHJpbmcgKGFjY291bnQ6c3RyaW5nIG5ldy1ndWFyZDpndWFyZClcXG5cXG4gICAgKHdpdGgtcmVhZCBjb2luLXRhYmxlIGFjY291bnRcXG4gICAgICB7IFxcXCJndWFyZFxcXCIgOj0gb2xkLWd1YXJkIH1cXG5cXG4gICAgICAoZW5mb3JjZS1ndWFyZCBvbGQtZ3VhcmQpXFxuICAgICAgKGVuZm9yY2UtZ3VhcmQgbmV3LWd1YXJkKVxcblxcbiAgICAgICh1cGRhdGUgY29pbi10YWJsZSBhY2NvdW50XFxuICAgICAgICB7IFxcXCJndWFyZFxcXCIgOiBuZXctZ3VhcmQgfVxcbiAgICAgICAgKSkpXFxuXFxuXFxuICAoZGVmdW4gcHJlY2lzaW9uOmludGVnZXJcXG4gICAgKClcXG4gICAgTUlOSU1VTV9QUkVDSVNJT04pXFxuXFxuICAoZGVmdW4gdHJhbnNmZXI6c3RyaW5nIChzZW5kZXI6c3RyaW5nIHJlY2VpdmVyOnN0cmluZyBhbW91bnQ6ZGVjaW1hbClcXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5IGNvbnNlcnZlcy1tYXNzKVxcbiAgICAgICAgICAgICAocHJvcGVydHkgKD4gYW1vdW50IDAuMCkpXFxuICAgICAgICAgICAgIChwcm9wZXJ0eSAodmFsaWQtYWNjb3VudCBzZW5kZXIpKVxcbiAgICAgICAgICAgICAocHJvcGVydHkgKHZhbGlkLWFjY291bnQgcmVjZWl2ZXIpKVxcbiAgICAgICAgICAgICAocHJvcGVydHkgKCE9IHNlbmRlciByZWNlaXZlcikpIF1cXG5cXG4gICAgKGVuZm9yY2UgKCE9IHNlbmRlciByZWNlaXZlcilcXG4gICAgICBcXFwic2VuZGVyIGNhbm5vdCBiZSB0aGUgcmVjZWl2ZXIgb2YgYSB0cmFuc2ZlclxcXCIpXFxuXFxuICAgICh2YWxpZGF0ZS1hY2NvdW50IHNlbmRlcilcXG4gICAgKHZhbGlkYXRlLWFjY291bnQgcmVjZWl2ZXIpXFxuXFxuICAgIChlbmZvcmNlICg-IGFtb3VudCAwLjApXFxuICAgICAgXFxcInRyYW5zZmVyIGFtb3VudCBtdXN0IGJlIHBvc2l0aXZlXFxcIilcXG5cXG4gICAgKGVuZm9yY2UtdW5pdCBhbW91bnQpXFxuXFxuICAgICh3aXRoLWNhcGFiaWxpdHkgKFRSQU5TRkVSIHNlbmRlciByZWNlaXZlciBhbW91bnQpXFxuICAgICAgKGRlYml0IHNlbmRlciBhbW91bnQpXFxuICAgICAgKHdpdGgtcmVhZCBjb2luLXRhYmxlIHJlY2VpdmVyXFxuICAgICAgICB7IFxcXCJndWFyZFxcXCIgOj0gZyB9XFxuXFxuICAgICAgICAoY3JlZGl0IHJlY2VpdmVyIGcgYW1vdW50KSlcXG4gICAgICApXFxuICAgIClcXG5cXG4gIChkZWZ1biB0cmFuc2Zlci1jcmVhdGU6c3RyaW5nXFxuICAgICggc2VuZGVyOnN0cmluZ1xcbiAgICAgIHJlY2VpdmVyOnN0cmluZ1xcbiAgICAgIHJlY2VpdmVyLWd1YXJkOmd1YXJkXFxuICAgICAgYW1vdW50OmRlY2ltYWwgKVxcblxcbiAgICBAbW9kZWwgWyAocHJvcGVydHkgY29uc2VydmVzLW1hc3MpIF1cXG5cXG4gICAgKGVuZm9yY2UgKCE9IHNlbmRlciByZWNlaXZlcilcXG4gICAgICBcXFwic2VuZGVyIGNhbm5vdCBiZSB0aGUgcmVjZWl2ZXIgb2YgYSB0cmFuc2ZlclxcXCIpXFxuXFxuICAgICh2YWxpZGF0ZS1hY2NvdW50IHNlbmRlcilcXG4gICAgKHZhbGlkYXRlLWFjY291bnQgcmVjZWl2ZXIpXFxuXFxuICAgIChlbmZvcmNlICg-IGFtb3VudCAwLjApXFxuICAgICAgXFxcInRyYW5zZmVyIGFtb3VudCBtdXN0IGJlIHBvc2l0aXZlXFxcIilcXG5cXG4gICAgKGVuZm9yY2UtdW5pdCBhbW91bnQpXFxuXFxuICAgICh3aXRoLWNhcGFiaWxpdHkgKFRSQU5TRkVSIHNlbmRlciByZWNlaXZlciBhbW91bnQpXFxuICAgICAgKGRlYml0IHNlbmRlciBhbW91bnQpXFxuICAgICAgKGNyZWRpdCByZWNlaXZlciByZWNlaXZlci1ndWFyZCBhbW91bnQpKVxcbiAgICApXFxuXFxuICAoZGVmdW4gY29pbmJhc2U6c3RyaW5nIChhY2NvdW50OnN0cmluZyBhY2NvdW50LWd1YXJkOmd1YXJkIGFtb3VudDpkZWNpbWFsKVxcbiAgICBAZG9jIFxcXCJJbnRlcm5hbCBmdW5jdGlvbiBmb3IgdGhlIGluaXRpYWwgY3JlYXRpb24gb2YgY29pbnMuICBUaGlzIGZ1bmN0aW9uIFxcXFxcXG4gICAgXFxcXGNhbm5vdCBiZSB1c2VkIG91dHNpZGUgb2YgdGhlIGNvaW4gY29udHJhY3QuXFxcIlxcblxcbiAgICBAbW9kZWwgWyAocHJvcGVydHkgKHZhbGlkLWFjY291bnQgYWNjb3VudCkpIF1cXG5cXG4gICAgKHZhbGlkYXRlLWFjY291bnQgYWNjb3VudClcXG4gICAgKGVuZm9yY2UtdW5pdCBhbW91bnQpXFxuXFxuICAgIChyZXF1aXJlLWNhcGFiaWxpdHkgKENPSU5CQVNFKSlcXG4gICAgKHdpdGgtY2FwYWJpbGl0eSAoQ1JFRElUIGFjY291bnQpXFxuICAgICAgKGNyZWRpdCBhY2NvdW50IGFjY291bnQtZ3VhcmQgYW1vdW50KSlcXG4gICAgKVxcblxcbiAgKGRlZnBhY3QgZnVuZC10eCAoc2VuZGVyOnN0cmluZyBtaW5lcjpzdHJpbmcgbWluZXItZ3VhcmQ6Z3VhcmQgdG90YWw6ZGVjaW1hbClcXG4gICAgQGRvYyBcXFwiJ2Z1bmQtdHgnIGlzIGEgc3BlY2lhbCBwYWN0IHRvIGZ1bmQgYSB0cmFuc2FjdGlvbiBpbiB0d28gc3RlcHMsICAgICBcXFxcXFxuICAgIFxcXFx3aXRoIHRoZSBhY3R1YWwgdHJhbnNhY3Rpb24gdHJhbnNwaXJpbmcgaW4gdGhlIG1pZGRsZTogICAgICAgICAgICAgICAgICAgXFxcXFxcbiAgICBcXFxcICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFxcXFxcXG4gICAgXFxcXCAgMSkgQSBidXlpbmcgcGhhc2UsIGRlYml0aW5nIHRoZSBzZW5kZXIgZm9yIHRvdGFsIGdhcyBhbmQgZmVlLCB5aWVsZGluZyBcXFxcXFxuICAgIFxcXFwgICAgIFRYX01BWF9DSEFSR0UuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXFxcXFxcbiAgICBcXFxcICAyKSBBIHNldHRsZW1lbnQgcGhhc2UsIHJlc3VtaW5nIFRYX01BWF9DSEFSR0UsIGFuZCBhbGxvY2F0aW5nIHRvIHRoZSAgIFxcXFxcXG4gICAgXFxcXCAgICAgY29pbmJhc2UgYWNjb3VudCBmb3IgdXNlZCBnYXMgYW5kIGZlZSwgYW5kIHNlbmRlciBhY2NvdW50IGZvciBiYWwtICBcXFxcXFxuICAgIFxcXFwgICAgIGFuY2UgKHVudXNlZCBnYXMsIGlmIGFueSkuXFxcIlxcblxcbiAgICBAbW9kZWwgWyAocHJvcGVydHkgKD4gdG90YWwgMC4wKSlcXG4gICAgICAgICAgICAgKHByb3BlcnR5ICh2YWxpZC1hY2NvdW50IHNlbmRlcikpXFxuICAgICAgICAgICAgIChwcm9wZXJ0eSAodmFsaWQtYWNjb3VudCBtaW5lcikpXFxuICAgICAgICAgICAgIDsocHJvcGVydHkgY29uc2VydmVzLW1hc3MpIG5vdCBzdXBwb3J0ZWQgeWV0XFxuICAgICAgICAgICBdXFxuXFxuICAgIChzdGVwIChidXktZ2FzIHNlbmRlciB0b3RhbCkpXFxuICAgIChzdGVwIChyZWRlZW0tZ2FzIG1pbmVyIG1pbmVyLWd1YXJkIHNlbmRlciB0b3RhbCkpXFxuICAgIClcXG5cXG4gIChkZWZ1biBkZWJpdDpzdHJpbmcgKGFjY291bnQ6c3RyaW5nIGFtb3VudDpkZWNpbWFsKVxcbiAgICBAZG9jIFxcXCJEZWJpdCBBTU9VTlQgZnJvbSBBQ0NPVU5UIGJhbGFuY2VcXFwiXFxuXFxuICAgIEBtb2RlbCBbIChwcm9wZXJ0eSAoPiBhbW91bnQgMC4wKSlcXG4gICAgICAgICAgICAgKHByb3BlcnR5ICh2YWxpZC1hY2NvdW50IGFjY291bnQpKVxcbiAgICAgICAgICAgXVxcblxcbiAgICAodmFsaWRhdGUtYWNjb3VudCBhY2NvdW50KVxcblxcbiAgICAoZW5mb3JjZSAoPiBhbW91bnQgMC4wKVxcbiAgICAgIFxcXCJkZWJpdCBhbW91bnQgbXVzdCBiZSBwb3NpdGl2ZVxcXCIpXFxuXFxuICAgIChlbmZvcmNlLXVuaXQgYW1vdW50KVxcblxcbiAgICAocmVxdWlyZS1jYXBhYmlsaXR5IChERUJJVCBhY2NvdW50KSlcXG4gICAgKHdpdGgtcmVhZCBjb2luLXRhYmxlIGFjY291bnRcXG4gICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6PSBiYWxhbmNlIH1cXG5cXG4gICAgICAoZW5mb3JjZSAoPD0gYW1vdW50IGJhbGFuY2UpIFxcXCJJbnN1ZmZpY2llbnQgZnVuZHNcXFwiKVxcblxcbiAgICAgICh1cGRhdGUgY29pbi10YWJsZSBhY2NvdW50XFxuICAgICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6ICgtIGJhbGFuY2UgYW1vdW50KSB9XFxuICAgICAgICApKVxcbiAgICApXFxuXFxuXFxuICAoZGVmdW4gY3JlZGl0OnN0cmluZyAoYWNjb3VudDpzdHJpbmcgZ3VhcmQ6Z3VhcmQgYW1vdW50OmRlY2ltYWwpXFxuICAgIEBkb2MgXFxcIkNyZWRpdCBBTU9VTlQgdG8gQUNDT1VOVCBiYWxhbmNlXFxcIlxcblxcbiAgICBAbW9kZWwgWyAocHJvcGVydHkgKD4gYW1vdW50IDAuMCkpXFxuICAgICAgICAgICAgIChwcm9wZXJ0eSAodmFsaWQtYWNjb3VudCBhY2NvdW50KSlcXG4gICAgICAgICAgIF1cXG5cXG4gICAgKHZhbGlkYXRlLWFjY291bnQgYWNjb3VudClcXG5cXG4gICAgKGVuZm9yY2UgKD4gYW1vdW50IDAuMCkgXFxcImNyZWRpdCBhbW91bnQgbXVzdCBiZSBwb3NpdGl2ZVxcXCIpXFxuICAgIChlbmZvcmNlLXVuaXQgYW1vdW50KVxcblxcbiAgICAocmVxdWlyZS1jYXBhYmlsaXR5IChDUkVESVQgYWNjb3VudCkpXFxuICAgICh3aXRoLWRlZmF1bHQtcmVhZCBjb2luLXRhYmxlIGFjY291bnRcXG4gICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6IDAuMCwgXFxcImd1YXJkXFxcIiA6IGd1YXJkIH1cXG4gICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6PSBiYWxhbmNlLCBcXFwiZ3VhcmRcXFwiIDo9IHJldGcgfVxcbiAgICAgIDsgd2UgZG9uJ3Qgd2FudCB0byBvdmVyd3JpdGUgYW4gZXhpc3RpbmcgZ3VhcmQgd2l0aCB0aGUgdXNlci1zdXBwbGllZCBvbmVcXG4gICAgICAoZW5mb3JjZSAoPSByZXRnIGd1YXJkKVxcbiAgICAgICAgXFxcImFjY291bnQgZ3VhcmRzIGRvIG5vdCBtYXRjaFxcXCIpXFxuXFxuICAgICAgKHdyaXRlIGNvaW4tdGFibGUgYWNjb3VudFxcbiAgICAgICAgeyBcXFwiYmFsYW5jZVxcXCIgOiAoKyBiYWxhbmNlIGFtb3VudClcXG4gICAgICAgICwgXFxcImd1YXJkXFxcIiAgIDogcmV0Z1xcbiAgICAgICAgfSlcXG4gICAgICApKVxcblxcblxcbiAgKGRlZnNjaGVtYSBjcm9zc2NoYWluLXNjaGVtYVxcbiAgICBAZG9jIFxcXCJTY2hlbWEgZm9yIHlpZWxkZWQgdmFsdWUgaW4gY3Jvc3MtY2hhaW4gdHJhbnNmZXJzXFxcIlxcbiAgICByZWNlaXZlcjpzdHJpbmdcXG4gICAgcmVjZWl2ZXItZ3VhcmQ6Z3VhcmRcXG4gICAgYW1vdW50OmRlY2ltYWwpXFxuXFxuICAoZGVmcGFjdCB0cmFuc2Zlci1jcm9zc2NoYWluOnN0cmluZ1xcbiAgICAoIHNlbmRlcjpzdHJpbmdcXG4gICAgICByZWNlaXZlcjpzdHJpbmdcXG4gICAgICByZWNlaXZlci1ndWFyZDpndWFyZFxcbiAgICAgIHRhcmdldC1jaGFpbjpzdHJpbmdcXG4gICAgICBhbW91bnQ6ZGVjaW1hbCApXFxuXFxuICAgIEBtb2RlbCBbIChwcm9wZXJ0eSAoPiBhbW91bnQgMC4wKSlcXG4gICAgICAgICAgICAgKHByb3BlcnR5ICghPSByZWNlaXZlciBcXFwiXFxcIikpXFxuICAgICAgICAgICAgIChwcm9wZXJ0eSAodmFsaWQtYWNjb3VudCBzZW5kZXIpKVxcbiAgICAgICAgICAgICAocHJvcGVydHkgKHZhbGlkLWFjY291bnQgcmVjZWl2ZXIpKVxcbiAgICAgICAgICAgXVxcblxcbiAgICAoc3RlcFxcbiAgICAgICh3aXRoLWNhcGFiaWxpdHkgKERFQklUIHNlbmRlcilcXG5cXG4gICAgICAgICh2YWxpZGF0ZS1hY2NvdW50IHNlbmRlcilcXG4gICAgICAgICh2YWxpZGF0ZS1hY2NvdW50IHJlY2VpdmVyKVxcblxcbiAgICAgICAgKGVuZm9yY2UgKCE9IFxcXCJcXFwiIHRhcmdldC1jaGFpbikgXFxcImVtcHR5IHRhcmdldC1jaGFpblxcXCIpXFxuICAgICAgICAoZW5mb3JjZSAoIT0gKGF0ICdjaGFpbi1pZCAoY2hhaW4tZGF0YSkpIHRhcmdldC1jaGFpbilcXG4gICAgICAgICAgXFxcImNhbm5vdCBydW4gY3Jvc3MtY2hhaW4gdHJhbnNmZXJzIHRvIHRoZSBzYW1lIGNoYWluXFxcIilcXG5cXG4gICAgICAgIChlbmZvcmNlICg-IGFtb3VudCAwLjApXFxuICAgICAgICAgIFxcXCJ0cmFuc2ZlciBxdWFudGl0eSBtdXN0IGJlIHBvc2l0aXZlXFxcIilcXG5cXG4gICAgICAgIChlbmZvcmNlLXVuaXQgYW1vdW50KVxcblxcbiAgICAgICAgOzsgc3RlcCAxIC0gZGViaXQgZGVsZXRlLWFjY291bnQgb24gY3VycmVudCBjaGFpblxcbiAgICAgICAgKGRlYml0IHNlbmRlciBhbW91bnQpXFxuXFxuICAgICAgICAobGV0XFxuICAgICAgICAgICgoY3Jvc3NjaGFpbi1kZXRhaWxzOm9iamVjdHtjcm9zc2NoYWluLXNjaGVtYX1cXG4gICAgICAgICAgICB7IFxcXCJyZWNlaXZlclxcXCIgOiByZWNlaXZlclxcbiAgICAgICAgICAgICwgXFxcInJlY2VpdmVyLWd1YXJkXFxcIiA6IHJlY2VpdmVyLWd1YXJkXFxuICAgICAgICAgICAgLCBcXFwiYW1vdW50XFxcIiA6IGFtb3VudFxcbiAgICAgICAgICAgIH0pKVxcbiAgICAgICAgICAoeWllbGQgY3Jvc3NjaGFpbi1kZXRhaWxzIHRhcmdldC1jaGFpbilcXG4gICAgICAgICAgKSkpXFxuXFxuICAgIChzdGVwXFxuICAgICAgKHJlc3VtZVxcbiAgICAgICAgeyBcXFwicmVjZWl2ZXJcXFwiIDo9IHJlY2VpdmVyXFxuICAgICAgICAsIFxcXCJyZWNlaXZlci1ndWFyZFxcXCIgOj0gcmVjZWl2ZXItZ3VhcmRcXG4gICAgICAgICwgXFxcImFtb3VudFxcXCIgOj0gYW1vdW50XFxuICAgICAgICB9XFxuXFxuICAgICAgICA7OyBzdGVwIDIgLSBjcmVkaXQgY3JlYXRlIGFjY291bnQgb24gdGFyZ2V0IGNoYWluXFxuICAgICAgICAod2l0aC1jYXBhYmlsaXR5IChDUkVESVQgcmVjZWl2ZXIpXFxuICAgICAgICAgIChjcmVkaXQgcmVjZWl2ZXIgcmVjZWl2ZXItZ3VhcmQgYW1vdW50KSlcXG4gICAgICAgICkpXFxuICAgIClcXG5cXG5cXG4gIDsgLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS1cXG4gIDsgQ29pbiBhbGxvY2F0aW9uc1xcblxcbiAgKGRlZnNjaGVtYSBhbGxvY2F0aW9uLXNjaGVtYVxcbiAgICBAZG9jIFxcXCJHZW5lc2lzIGFsbG9jYXRpb24gcmVnaXN0cnlcXFwiXFxuICAgIDtAbW9kZWwgWyAoaW52YXJpYW50ICg-PSBiYWxhbmNlIDAuMCkpIF1cXG5cXG4gICAgYmFsYW5jZTpkZWNpbWFsXFxuICAgIGRhdGU6dGltZVxcbiAgICBndWFyZDpndWFyZFxcbiAgICByZWRlZW1lZDpib29sKVxcblxcbiAgKGRlZnRhYmxlIGFsbG9jYXRpb24tdGFibGU6e2FsbG9jYXRpb24tc2NoZW1hfSlcXG5cXG4gIChkZWZ1biBjcmVhdGUtYWxsb2NhdGlvbi1hY2NvdW50XFxuICAgICggYWNjb3VudDpzdHJpbmdcXG4gICAgICBkYXRlOnRpbWVcXG4gICAgICBrZXlzZXQtcmVmOnN0cmluZ1xcbiAgICAgIGFtb3VudDpkZWNpbWFsXFxuICAgIClcXG5cXG4gICAgQGRvYyBcXFwiQWRkIGFuIGVudHJ5IHRvIHRoZSBjb2luIGFsbG9jYXRpb24gdGFibGUuIFRoaXMgZnVuY3Rpb24gXFxcXFxcbiAgICAgICAgIFxcXFxhbHNvIGNyZWF0ZXMgYSBjb3JyZXNwb25kaW5nIGVtcHR5IGNvaW4gY29udHJhY3QgYWNjb3VudCBcXFxcXFxuICAgICAgICAgXFxcXG9mIHRoZSBzYW1lIG5hbWUgYW5kIGd1YXJkLiBSZXF1aXJlcyBHRU5FU0lTIGNhcGFiaWxpdHkuIFxcXCJcXG5cXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5ICh2YWxpZC1hY2NvdW50IGFjY291bnQpKSBdXFxuXFxuICAgIChyZXF1aXJlLWNhcGFiaWxpdHkgKEdFTkVTSVMpKVxcblxcbiAgICAodmFsaWRhdGUtYWNjb3VudCBhY2NvdW50KVxcbiAgICAoZW5mb3JjZSAoPj0gYW1vdW50IDAuMClcXG4gICAgICBcXFwiYWxsb2NhdGlvbiBhbW91bnQgbXVzdCBiZSBub24tbmVnYXRpdmVcXFwiKVxcblxcbiAgICAoZW5mb3JjZS11bml0IGFtb3VudClcXG5cXG4gICAgKGxldFxcbiAgICAgICgoZ3VhcmQ6Z3VhcmQgKGtleXNldC1yZWYtZ3VhcmQga2V5c2V0LXJlZikpKVxcblxcbiAgICAgIChjcmVhdGUtYWNjb3VudCBhY2NvdW50IGd1YXJkKVxcblxcbiAgICAgIChpbnNlcnQgYWxsb2NhdGlvbi10YWJsZSBhY2NvdW50XFxuICAgICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6IGFtb3VudFxcbiAgICAgICAgLCBcXFwiZGF0ZVxcXCIgOiBkYXRlXFxuICAgICAgICAsIFxcXCJndWFyZFxcXCIgOiBndWFyZFxcbiAgICAgICAgLCBcXFwicmVkZWVtZWRcXFwiIDogZmFsc2VcXG4gICAgICAgIH0pKSlcXG5cXG4gIChkZWZ1biByZWxlYXNlLWFsbG9jYXRpb25cXG4gICAgKCBhY2NvdW50OnN0cmluZyApXFxuXFxuICAgIEBkb2MgXFxcIlJlbGVhc2UgZnVuZHMgYXNzb2NpYXRlZCB3aXRoIGFsbG9jYXRpb24gQUNDT1VOVCBpbnRvIG1haW4gbGVkZ2VyLiAgIFxcXFxcXG4gICAgICAgICBcXFxcQUNDT1VOVCBtdXN0IGFscmVhZHkgZXhpc3QgaW4gbWFpbiBsZWRnZXIuIEFsbG9jYXRpb24gaXMgZGVhY3RpdmF0ZWQgXFxcXFxcbiAgICAgICAgIFxcXFxhZnRlciByZWxlYXNlLlxcXCJcXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5ICh2YWxpZC1hY2NvdW50IGFjY291bnQpKSBdXFxuXFxuICAgICh2YWxpZGF0ZS1hY2NvdW50IGFjY291bnQpXFxuXFxuICAgICh3aXRoLXJlYWQgYWxsb2NhdGlvbi10YWJsZSBhY2NvdW50XFxuICAgICAgeyBcXFwiYmFsYW5jZVxcXCIgOj0gYmFsYW5jZVxcbiAgICAgICwgXFxcImRhdGVcXFwiIDo9IHJlbGVhc2UtdGltZVxcbiAgICAgICwgXFxcInJlZGVlbWVkXFxcIiA6PSByZWRlZW1lZFxcbiAgICAgICwgXFxcImd1YXJkXFxcIiA6PSBndWFyZFxcbiAgICAgIH1cXG5cXG4gICAgICAobGV0ICgoY3Vyci10aW1lOnRpbWUgKGF0ICdibG9jay10aW1lIChjaGFpbi1kYXRhKSkpKVxcblxcbiAgICAgICAgKGVuZm9yY2UgKG5vdCByZWRlZW1lZClcXG4gICAgICAgICAgXFxcImFsbG9jYXRpb24gZnVuZHMgaGF2ZSBhbHJlYWR5IGJlZW4gcmVkZWVtZWRcXFwiKVxcblxcbiAgICAgICAgKGVuZm9yY2VcXG4gICAgICAgICAgKD49IGN1cnItdGltZSByZWxlYXNlLXRpbWUpXFxuICAgICAgICAgIChmb3JtYXQgXFxcImZ1bmRzIGxvY2tlZCB1bnRpbCB7fS4gY3VycmVudCB0aW1lOiB7fVxcXCIgW3JlbGVhc2UtdGltZSBjdXJyLXRpbWVdKSlcXG5cXG4gICAgICAgIChlbmZvcmNlLWd1YXJkIGd1YXJkKVxcblxcbiAgICAgICAgKHdpdGgtY2FwYWJpbGl0eSAoQ1JFRElUIGFjY291bnQpXFxuICAgICAgICAgIChjcmVkaXQgYWNjb3VudCBndWFyZCBiYWxhbmNlKVxcblxcbiAgICAgICAgICAodXBkYXRlIGFsbG9jYXRpb24tdGFibGUgYWNjb3VudFxcbiAgICAgICAgICAgIHsgXFxcInJlZGVlbWVkXFxcIiA6IHRydWVcXG4gICAgICAgICAgICAsIFxcXCJiYWxhbmNlXFxcIiA6IDAuMFxcbiAgICAgICAgICAgIH0pXFxuXFxuICAgICAgICAgIFxcXCJBbGxvY2F0aW9uIHN1Y2Nlc3NmdWxseSByZWxlYXNlZCB0byBtYWluIGxlZGdlclxcXCIpXFxuICAgICkpKVxcblxcbilcXG5cXG4oY3JlYXRlLXRhYmxlIGNvaW4tdGFibGUpXFxuKGNyZWF0ZS10YWJsZSBhbGxvY2F0aW9uLXRhYmxlKVxcblwifX0sXCJzaWduZXJzXCI6W10sXCJtZXRhXCI6e1wiY3JlYXRpb25UaW1lXCI6MCxcInR0bFwiOjE3MjgwMCxcImdhc0xpbWl0XCI6MCxcImNoYWluSWRcIjpcIlwiLFwiZ2FzUHJpY2VcIjowLFwic2VuZGVyXCI6XCJcIn0sXCJub25jZVwiOlwiZ2VuZXNpcy0wMVwifSJ9
  - eyJnYXMiOjAsInJlc3VsdCI6eyJzdGF0dXMiOiJzdWNjZXNzIiwiZGF0YSI6IlRhYmxlQ3JlYXRlZCJ9LCJyZXFLZXkiOiJYR1BFUURrNVBJdlFrcHEwR0drZ05UbW8tbWpraTYzWlBnRVJfa292eHE0IiwibG9ncyI6IkpvNXhfMEl0dnZLVTY5dHJFbjh1a3FCeWpSTy1XaEZJZFlmMDV5dDlfN0UiLCJtZXRhRGF0YSI6bnVsbCwiY29udGludWF0aW9uIjpudWxsLCJ0eElkIjoxfQ
- - eyJoYXNoIjoiWlB6eHBLMFRyYVlNWXBZRzVsWWJIanpaeW9WYVZUZU1qZlE5S3hVX0lsYyIsInNpZ3MiOlt7InNpZyI6IjdjMjgxYWU1MDAwMjNiZDIzYjQ0MDhmMDNiNjNlNjU2ZTE0MzNkOWY4YWFhYjkyNDc4NjcxZDIwZWNlNDMyODUwNGQ4NGFmMjI2ZTExMTMyYTM3ZTM2MTM0YmU3ZDYzMDcxZjlhZDBlZWMzYWZiNzUwZjJkMThhMzg1M2QyYzBmIn1dLCJjbWQiOiJ7XCJuZXR3b3JrSWRcIjpudWxsLFwicGF5bG9hZFwiOntcImV4ZWNcIjp7XCJkYXRhXCI6e1wibnMtYWRtaW4ta2V5c2V0XCI6W1wiMzY4ODIwZjgwYzMyNGJiYzdjMmIwNjEwNjg4YTdkYTQzZTM5ZjkxZDExODczMjY3MWNkOWM3NTAwZmY0M2NjYVwiXSxcIm5zLW9wZXJhdGUta2V5c2V0XCI6W1wiMzY4ODIwZjgwYzMyNGJiYzdjMmIwNjEwNjg4YTdkYTQzZTM5ZjkxZDExODczMjY3MWNkOWM3NTAwZmY0M2NjYVwiXX0sXCJjb2RlXCI6XCJcXG4oZGVmaW5lLWtleXNldCAnbnMtYWRtaW4ta2V5c2V0IChyZWFkLWtleXNldCAnbnMtYWRtaW4ta2V5c2V0KSlcXG4oZGVmaW5lLWtleXNldCAnbnMtb3BlcmF0ZS1rZXlzZXQgKHJlYWQta2V5c2V0ICducy1vcGVyYXRlLWtleXNldCkpXFxuXFxuKG1vZHVsZSBucyAnbnMtYWRtaW4ta2V5c2V0XFxuICBcXFwiQWRtaW5pc3RlcnMgZGVmaW5pdGlvbiBvZiBuZXcgbmFtZXNwYWNlcyBpbiBDaGFpbndlYi5cXFwiXFxuXFxuICAoZGVmc2NoZW1hIHJlZy1lbnRyeVxcbiAgICBhZG1pbi1ndWFyZDpndWFyZFxcbiAgICBhY3RpdmU6Ym9vbClcXG5cXG4gIChkZWZ0YWJsZSByZWdpc3RyeTp7cmVnLWVudHJ5fSlcXG5cXG4gIChkZWZjYXAgT1BFUkFURSAoKVxcbiAgICAoZW5mb3JjZS1rZXlzZXQgJ25zLW9wZXJhdGUta2V5c2V0KSlcXG5cXG4gIChkZWZjb25zdCBHVUFSRF9TVUNDRVNTIChjcmVhdGUtdXNlci1ndWFyZCAoc3VjY2VzcykpKVxcbiAgKGRlZmNvbnN0IEdVQVJEX0ZBSUxVUkUgKGNyZWF0ZS11c2VyLWd1YXJkIChmYWlsdXJlKSkpXFxuXFxuICAoZGVmdW4gc3VjY2VzcyAoKVxcbiAgICB0cnVlKVxcbiAgKGRlZnVuIGZhaWx1cmUgKClcXG4gICAgKGVuZm9yY2UgZmFsc2UgXFxcIkRpc2FibGVkXFxcIikpXFxuXFxuICAoZGVmdW4gdmFsaWRhdGUtbmFtZSAobmFtZSlcXG4gICAgKGVuZm9yY2UgKCE9IFxcXCJcXFwiIG5hbWUpIFxcXCJFbXB0eSBuYW1lIG5vdCBhbGxvd2VkXFxcIilcXG4gICAgKGVuZm9yY2UgKDwgKGxlbmd0aCBuYW1lKSA2NCkgXFxcIk5hbWUgbXVzdCBiZSBsZXNzIHRoYW4gNjQgY2hhcmFjdGVycyBsb25nXFxcIilcXG4gICAgKGVuZm9yY2UgKGlzLWNoYXJzZXQgQ0hBUlNFVF9MQVRJTjEgbmFtZSlcXG4gICAgICAgICAgICAgXFxcIk5hbWUgbXVzdCBiZSBpbiBsYXRpbjEgY2hhcnNldFxcXCIpKVxcblxcbiAgKGRlZnVuIHZhbGlkYXRlOmJvb2xcXG4gICAgICAoIG5zLW5hbWU6c3RyaW5nXFxuICAgICAgICBucy1hZG1pbjpndWFyZFxcbiAgICAgICAgKVxcbiAgICBcXFwiIE1hbmFnZXMgbmFtZXNwYWNlIGluc3RhbGwgZm9yIENoYWlud2ViLiBSZXF1aXJlcyBhY3RpdmUgcm93IGluIHJlZ2lzdHJ5IFxcXFxcXG4gICAgXFxcXCBmb3IgTlMtTkFNRSB3aXRoIGd1YXJkIG1hdGNoaW5nIE5TLUFETUlOLlxcXCJcXG5cXG4gICAgKHZhbGlkYXRlLW5hbWUgbnMtbmFtZSlcXG5cXG4gICAgKHdpdGgtZGVmYXVsdC1yZWFkIHJlZ2lzdHJ5IG5zLW5hbWVcXG4gICAgICB7ICdhZG1pbi1ndWFyZCA6IG5zLWFkbWluXFxuICAgICAgLCAnYWN0aXZlIDogZmFsc2UgfVxcbiAgICAgIHsgJ2FkbWluLWd1YXJkIDo9IGFnXFxuICAgICAgLCAnYWN0aXZlIDo9IGlzLWFjdGl2ZSB9XFxuXFxuICAgICAgICAoZW5mb3JjZSBpcy1hY3RpdmUgXFxcIkluYWN0aXZlIG9yIHVucmVnaXN0ZXJlZCBuYW1lc3BhY2VcXFwiKVxcbiAgICAgICAgKGVuZm9yY2UgKD0gbnMtYWRtaW4gYWcpIFxcXCJBZG1pbiBndWFyZCBtdXN0IG1hdGNoIGd1YXJkIGluIHJlZ2lzdHJ5XFxcIilcXG5cXG4gICAgICAgIHRydWUpKVxcblxcbiAgKGRlZnVuIHdyaXRlLXJlZ2lzdHJ5OnN0cmluZ1xcbiAgICAgICggbnMtbmFtZTpzdHJpbmdcXG4gICAgICAgIGd1YXJkOmd1YXJkXFxuICAgICAgICBhY3RpdmU6Ym9vbFxcbiAgICAgICAgKVxcbiAgICBcXFwiIFdyaXRlIGVudHJ5IHdpdGggR1VBUkQgYW5kIEFDVElWRSBpbnRvIHJlZ2lzdHJ5IGZvciBOQU1FLiBcXFxcXFxuICAgIFxcXFwgR3VhcmRlZCBieSBvcGVyYXRlIGtleXNldC4gXFxcIlxcblxcbiAgICAod2l0aC1jYXBhYmlsaXR5IChPUEVSQVRFKVxcblxcbiAgICAgICh2YWxpZGF0ZS1uYW1lIG5zLW5hbWUpXFxuXFxuICAgICAgKHdyaXRlIHJlZ2lzdHJ5IG5zLW5hbWVcXG4gICAgICAgIHsgJ2FkbWluLWd1YXJkOiBndWFyZFxcbiAgICAgICAgLCAnYWN0aXZlOiBhY3RpdmUgfSlcXG5cXG4gICAgICBcXFwiUmVnaXN0ZXIgZW50cnkgd3JpdHRlblxcXCIpKVxcblxcbiAgKGRlZnVuIHF1ZXJ5Om9iamVjdHtyZWctZW50cnl9XFxuICAgICAgKCBucy1uYW1lOnN0cmluZyApXFxuICAgIChyZWFkIHJlZ2lzdHJ5IG5zLW5hbWUpKVxcblxcbiAgKVxcblxcbihjcmVhdGUtdGFibGUgcmVnaXN0cnkpXFxuXFxuKHdyaXRlLXJlZ2lzdHJ5IFxcXCJrYWRlbmFcXFwiXFxuICAoa2V5c2V0LXJlZi1ndWFyZCAnbnMtb3BlcmF0ZS1rZXlzZXQpIHRydWUpXFxuKHdyaXRlLXJlZ2lzdHJ5IFxcXCJ1c2VyXFxcIiBHVUFSRF9GQUlMVVJFIHRydWUpXFxuKHdyaXRlLXJlZ2lzdHJ5IFxcXCJmcmVlXFxcIiBHVUFSRF9GQUlMVVJFIHRydWUpXFxuXFxuKGRlZmluZS1uYW1lc3BhY2UgXFxcImthZGVuYVxcXCJcXG4gIChrZXlzZXQtcmVmLWd1YXJkICducy1vcGVyYXRlLWtleXNldClcXG4gIChrZXlzZXQtcmVmLWd1YXJkICducy1vcGVyYXRlLWtleXNldCkpXFxuXFxuKGRlZmluZS1uYW1lc3BhY2UgXFxcInVzZXJcXFwiIEdVQVJEX1NVQ0NFU1MgR1VBUkRfRkFJTFVSRSlcXG4oZGVmaW5lLW5hbWVzcGFjZSBcXFwiZnJlZVxcXCIgR1VBUkRfU1VDQ0VTUyBHVUFSRF9GQUlMVVJFKVxcblwifX0sXCJzaWduZXJzXCI6W3tcInB1YktleVwiOlwiMzY4ODIwZjgwYzMyNGJiYzdjMmIwNjEwNjg4YTdkYTQzZTM5ZjkxZDExODczMjY3MWNkOWM3NTAwZmY0M2NjYVwifV0sXCJtZXRhXCI6e1wiY3JlYXRpb25UaW1lXCI6MCxcInR0bFwiOjE3MjgwMCxcImdhc0xpbWl0XCI6MCxcImNoYWluSWRcIjpcIlwiLFwiZ2FzUHJpY2VcIjowLFwic2VuZGVyXCI6XCJcIn0sXCJub25jZVwiOlwibG9hZC1ucy10ZXN0bmV0LXNlbmRlcjAwXCJ9In0
  - eyJnYXMiOjAsInJlc3VsdCI6eyJzdGF0dXMiOiJzdWNjZXNzIiwiZGF0YSI6Ik5hbWVzcGFjZSBkZWZpbmVkOiBmcmVlIn0sInJlcUtleSI6IlpQenhwSzBUcmFZTVlwWUc1bFliSGp6WnlvVmFWVGVNamZROUt4VV9JbGMiLCJsb2dzIjoiRGpTbTlEWEZSZGo4RjVGSXpPOWpMNUVUbDFuNGhYN3c2NHhiRGRGb1VJayIsIm1ldGFEYXRhIjpudWxsLCJjb250aW51YXRpb24iOm51bGwsInR4SWQiOjJ9
- - eyJoYXNoIjoidWpyZFVCSkt0UTNlNk9tTm9OVUdtWGlUWHJwdTMxbklCU085WVA3MHZ2ayIsInNpZ3MiOltdLCJjbWQiOiJ7XCJuZXR3b3JrSWRcIjpudWxsLFwicGF5bG9hZFwiOntcImV4ZWNcIjp7XCJkYXRhXCI6e1wic2VuZGVyMDdcIjpbXCI0YzMxZGM5ZWU3ZjI0MTc3Zjc4YjZmNTE4MDEyYTIwODMyNmUyYWYxZjM3YmIwYTI0MDViNTA1NmQwY2FkNjI4XCJdLFwic2VuZGVyMDFcIjpbXCI2YmUyZjQ4NWE3YWY3NWZlZGI0YjdmMTUzYTkwM2Y3ZTYwMDBjYTRhYTUwMTE3OWM5MWEyNDUwYjc3N2JkMmE3XCJdLFwic2VuZGVyMDZcIjpbXCI1ZmZjMWY3ZmVmN2E0NDczODYyNTc2MmY3NWE0MjI5NDU0OTUxZTAzZjJhZmM2ZjgxMzA5YzBjMWJkZjllZTZmXCJdLFwic2VuZGVyMDBcIjpbXCIzNjg4MjBmODBjMzI0YmJjN2MyYjA2MTA2ODhhN2RhNDNlMzlmOTFkMTE4NzMyNjcxY2Q5Yzc1MDBmZjQzY2NhXCJdLFwic2VuZGVyMDVcIjpbXCJmMDlkOGY2Mzk0YWVhNDI1ZmU2NzgzZDg4Y2Q4MTM2M2Q4MDE3ZjE2YWZkMzcxMWM1NzViZTBmNWNkNWM5YmI5XCJdLFwic2VuZGVyMDRcIjpbXCIyZDcwYWE0ZjY5N2MzYTNiOGRkNmQ5Nzc0NWFjMDc0ZWRjZmQwZWI2NWMzNzc3NGNkZTI1MTM1NDgzYmVhNzFlXCJdLFwibXVsdGktMDItMDMtMDQtYW55XCI6e1wicHJlZFwiOlwia2V5cy1hbnlcIixcImtleXNcIjpbXCIzYTlkZDUzMmQ3M2RhY2UxOTVkYmI2NGQxZGJhNjU3MmZiNzgzZDBmZGQzMjQ2ODVlMzJmYmRhMmY4OWY5OWE2XCIsXCI0M2YyYWRiMWRlMTkyMDAwY2IzNzc3YmFjYzdmOTgzYjY2MTRmZDljMTcxNWNkNDRjZDQ4NGI2ZDNhMGQzNGM4XCIsXCIyZDcwYWE0ZjY5N2MzYTNiOGRkNmQ5Nzc0NWFjMDc0ZWRjZmQwZWI2NWMzNzc3NGNkZTI1MTM1NDgzYmVhNzFlXCJdfSxcInNlbmRlcjA5XCI6W1wiYzU5ZDk4NDBiMGI2NjA5MDgzNjU0NmI3ZWI0YTczNjA2MjU3NTI3ZWM4YzJiNDgyMzAwZmQyMjkyNjRiMDdlNlwiXSxcInNlbmRlcjAzXCI6W1wiNDNmMmFkYjFkZTE5MjAwMGNiMzc3N2JhY2M3Zjk4M2I2NjE0ZmQ5YzE3MTVjZDQ0Y2Q0ODRiNmQzYTBkMzRjOFwiXSxcIm11bHRpLTAwLTAxXCI6W1wiMzY4ODIwZjgwYzMyNGJiYzdjMmIwNjEwNjg4YTdkYTQzZTM5ZjkxZDExODczMjY3MWNkOWM3NTAwZmY0M2NjYVwiLFwiNmJlMmY0ODVhN2FmNzVmZWRiNGI3ZjE1M2E5MDNmN2U2MDAwY2E0YWE1MDExNzljOTFhMjQ1MGI3NzdiZDJhN1wiXSxcInNlbmRlcjA4XCI6W1wiNjNiMmViYTRlZDcwZDQ2MTJkM2U3YmM5MGRiMmZiZjRjNzZmN2IwNzQzNjNlODZkNzNmMGJjNjE3ZjhlOGI4MVwiXSxcInNlbmRlcjAyXCI6W1wiM2E5ZGQ1MzJkNzNkYWNlMTk1ZGJiNjRkMWRiYTY1NzJmYjc4M2QwZmRkMzI0Njg1ZTMyZmJkYTJmODlmOTlhNlwiXX0sXCJjb2RlXCI6XCIoY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDBcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDBcXFwiKSAxMDAwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDFcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDFcXFwiKSAxMTAwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDJcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDJcXFwiKSAxMjAwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDNcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDNcXFwiKSAxMzAwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDRcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDRcXFwiKSAxNDAwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDVcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDVcXFwiKSAxNTAwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDZcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDZcXFwiKSAxNjAwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDdcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDdcXFwiKSAxNzAwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDhcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDhcXFwiKSAxODAwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDlcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDlcXFwiKSAxOTAwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwibXVsdGktMDAtMDFcXFwiIChyZWFkLWtleXNldCBcXFwibXVsdGktMDAtMDFcXFwiKSAxMDEwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwibXVsdGktMDItMDMtMDQtYW55XFxcIiAocmVhZC1rZXlzZXQgXFxcIm11bHRpLTAyLTAzLTA0LWFueVxcXCIpIDEyMzQwMDAwMC4wKVwifX0sXCJzaWduZXJzXCI6W10sXCJtZXRhXCI6e1wiY3JlYXRpb25UaW1lXCI6MCxcInR0bFwiOjE3MjgwMCxcImdhc0xpbWl0XCI6MCxcImNoYWluSWRcIjpcIlwiLFwiZ2FzUHJpY2VcIjowLFwic2VuZGVyXCI6XCJcIn0sXCJub25jZVwiOlwidGVzdG5ldC1ncmFudHNcIn0ifQ
  - eyJnYXMiOjAsInJlc3VsdCI6eyJzdGF0dXMiOiJzdWNjZXNzIiwiZGF0YSI6IldyaXRlIHN1Y2NlZWRlZCJ9LCJyZXFLZXkiOiJ1anJkVUJKS3RRM2U2T21Ob05VR21YaVRYcnB1MzFuSUJTTzlZUDcwdnZrIiwibG9ncyI6IndOT0ItemVUbW81ZHkyVWJDWTM5cEgzaWw2SUFPM2ljeUxMclRCTzdxNlkiLCJtZXRhRGF0YSI6bnVsbCwiY29udGludWF0aW9uIjpudWxsLCJ0eElkIjozfQ
minerData: eyJhY2NvdW50IjoiTm9NaW5lciIsInByZWRpY2F0ZSI6IjwiLCJwdWJsaWMta2V5cyI6W119
transactionsHash: iuMLxYB63jzTD7-_CC2hETQLG1MOsiasNiW005ILfBA
outputsHash: OuBW8c7Kt6ZErSNzNUEAWC_EFWEDEnAHuq-j4aLZ5nc
payloadHash: aYjsD9yWOYkM-wrvIDreipMgmK98p3aC-TBwn5PaBDU
coinbase: eyJnYXMiOjAsInJlc3VsdCI6eyJzdGF0dXMiOiJzdWNjZXNzIiwiZGF0YSI6Ik5PX0NPSU5CQVNFIn0sInJlcUtleSI6IkRsZFJ3Q2JsUTdMb3F5NndZSm5hb2RIbDMwZDNqM2VILXF0RnpmRXY0NmciLCJsb2dzIjpudWxsLCJtZXRhRGF0YSI6bnVsbCwiY29udGludWF0aW9uIjpudWxsLCJ0eElkIjpudWxsfQ

|]
