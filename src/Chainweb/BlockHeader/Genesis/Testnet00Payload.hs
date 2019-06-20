{-# LANGUAGE QuasiQuotes #-}

-- This module is auto-generated. DO NOT EDIT IT MANUALLY.

module Chainweb.BlockHeader.Genesis.Testnet00Payload ( payloadBlock ) where

import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (decodeThrow)

import NeatInterpolation (text)

import Chainweb.Payload (PayloadWithOutputs)
import Chainweb.Utils (fromJuste)

payloadBlock :: PayloadWithOutputs
payloadBlock = fromJuste $ decodeThrow $ encodeUtf8 [text|
transactions:
- - eyJoYXNoIjoiNnp0RnJYY012ejdnb0FyUWZhckdIeUs5bFNjcU0xdGVHRnZyMmRwZlQ5SSIsInNpZ3MiOltdLCJjbWQiOiJ7XCJwYXlsb2FkXCI6e1wiZXhlY1wiOntcImRhdGFcIjpudWxsLFwiY29kZVwiOlwiXFxuKGludGVyZmFjZSBjb2luLXNpZ1xcblxcbiAgXFxcIidjb2luLXNpZycgcmVwcmVzZW50cyB0aGUgS2FkZW5hIENvaW4gQ29udHJhY3QgaW50ZXJmYWNlLiBUaGlzIGNvbnRyYWN0ICAgICBcXFxcXFxuICBcXFxccHJvdmlkZXMgYm90aCB0aGUgdGhlIGdlbmVyYWwgaW50ZXJmYWNlIGZvciBhIEthZGVuYSdzIHRva2VuLCBzdXBwbHlpbmcgYSAgIFxcXFxcXG4gIFxcXFx0cmFuc2ZlciBmdW5jdGlvbiwgY29pbmJhc2UsIGFjY291bnQgY3JlYXRpb24gYW5kIGJhbGFuY2UgcXVlcnkuXFxcIlxcblxcbiAgKGRlZnVuIGNyZWF0ZS1hY2NvdW50OnN0cmluZyAoYWNjb3VudDpzdHJpbmcgZ3VhcmQ6Z3VhcmQpXFxuICAgIEBkb2MgXFxcIkNyZWF0ZSBhbiBhY2NvdW50IGZvciBBQ0NPVU5ULCB3aXRoIEFDQ09VTlQgYXMgYSBmdW5jdGlvbiBvZiBHVUFSRFxcXCIpXFxuXFxuICAoZGVmdW4gdHJhbnNmZXI6c3RyaW5nIChzZW5kZXI6c3RyaW5nIHJlY2lldmVyOnN0cmluZyByZWNlaXZlci1ndWFyZDpndWFyZCBhbW91bnQ6ZGVjaW1hbClcXG4gICAgQGRvYyBcXFwiVHJhbnNmZXIgYmV0d2VlbiBhY2NvdW50cyBTRU5ERVIgYW5kIFJFQ0VJVkVSIG9uIHRoZSBzYW1lIGNoYWluLiAgICBcXFxcXFxuICAgIFxcXFxUaGlzIGZhaWxzIGlmIGJvdGggYWNjb3VudHMgZG8gbm90IGV4aXN0LiBDcmVhdGUtb24tdHJhbnNmZXIgY2FuIGJlICAgICAgXFxcXFxcbiAgICBcXFxcaGFuZGxlZCBieSBzZW5kaW5nIGluIGEgY3JlYXRlIGNvbW1hbmQgaW4gdGhlIHNhbWUgdHguXFxcIlxcblxcbiAgICBAbW9kZWwgWyAocHJvcGVydHkgKD4gYW1vdW50IDAuMCkpIF0pXFxuXFxuXFxuICAoZGVmdW4gYWNjb3VudC1iYWxhbmNlOmRlY2ltYWwgKGFjY291bnQ6c3RyaW5nKVxcbiAgICBAZG9jIFxcXCJRdWVyeSB1c2VyIGFjY291bnQgQUNDT1VOVCBiYWxhbmNlXFxcIilcXG5cXG4gIChkZWZ1biBjb2luYmFzZTpzdHJpbmcgKGFkZHJlc3M6c3RyaW5nIGFkZHJlc3MtZ3VhcmQ6Z3VhcmQgYW1vdW50OmRlY2ltYWwpXFxuICAgIEBkb2MgXFxcIk1pbnQgc29tZSBudW1iZXIgb2YgdG9rZW5zIGFuZCBhbGxvY2F0ZSB0aGVtIHRvIHNvbWUgYWRkcmVzc1xcXCJcXG5cXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5ICg-IGFtb3VudCAwLjApKSBdKVxcblxcbilcXG5cIn19LFwic2lnbmVyc1wiOltdLFwibWV0YVwiOntcImdhc0xpbWl0XCI6MCxcImNoYWluSWRcIjpcIlwiLFwiZ2FzUHJpY2VcIjowLFwic2VuZGVyXCI6XCJcIn0sXCJub25jZVwiOlwiXFxcImdlbmVzaXMtMDFcXFwiXCJ9In0
  - eyJnYXMiOjAsInJlc3VsdCI6eyJzdGF0dXMiOiJzdWNjZXNzIiwiZGF0YSI6IkxvYWRlZCBpbnRlcmZhY2UgY29pbi1zaWcifSwicmVxS2V5IjoiNnp0RnJYY012ejdnb0FyUWZhckdIeUs5bFNjcU0xdGVHRnZyMmRwZlQ5SSIsImxvZ3MiOiJobzlqWmIyYldNNWhaWG1YbEdXam56QzVjMWI5eWVyOGNBWmc2ZHZCZk93IiwibWV0YURhdGEiOm51bGwsImNvbnRpbnVhdGlvbiI6bnVsbCwidHhJZCI6MH0
- - eyJoYXNoIjoieE1ITzhsQVBYdldVdkhVYk1FT29JM2JpMHFOSTV3M0VUQ1VpNWpTZmZPTSIsInNpZ3MiOltdLCJjbWQiOiJ7XCJwYXlsb2FkXCI6e1wiZXhlY1wiOntcImRhdGFcIjpudWxsLFwiY29kZVwiOlwiKG1vZHVsZSBjb2luIEdPVkVSTkFOQ0VcXG5cXG4gIFxcXCInY29pbicgcmVwcmVzZW50cyB0aGUgS2FkZW5hIENvaW4gQ29udHJhY3QuIFRoaXMgY29udHJhY3QgcHJvdmlkZXMgYm90aCB0aGUgXFxcXFxcbiAgXFxcXGJ1eS9yZWRlZW0gZ2FzIHN1cHBvcnQgaW4gdGhlIGZvcm0gb2YgJ2Z1bmQtdHgnLCBhcyB3ZWxsIGFzIHRyYW5zZmVyLCAgICAgICBcXFxcXFxuICBcXFxcY3JlZGl0LCBkZWJpdCwgY29pbmJhc2UsIGFjY291bnQgY3JlYXRpb24gYW5kIHF1ZXJ5LCBhcyB3ZWxsIGFzIFNQViBidXJuICAgIFxcXFxcXG4gIFxcXFxjcmVhdGUuIFRvIGFjY2VzcyB0aGUgY29pbiBjb250cmFjdCwgeW91IG1heSB1c2UgaXRzIGZ1bGx5LXF1YWxpZmllZCBuYW1lLCAgXFxcXFxcbiAgXFxcXG9yIGlzc3VlIHRoZSAnKHVzZSBjb2luKScgY29tbWFuZCBpbiB0aGUgYm9keSBvZiBhIG1vZHVsZSBkZWNsYXJhdGlvbi5cXFwiXFxuXFxuXFxuICAodXNlIGNvaW4tc2lnKVxcblxcbiAgOyAtLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLVxcbiAgOyBTY2hlbWFzIGFuZCBUYWJsZXNcXG5cXG4gIChkZWZzY2hlbWEgY29pbi1zY2hlbWFcXG4gICAgYmFsYW5jZTpkZWNpbWFsXFxuICAgIGd1YXJkOmd1YXJkKVxcbiAgKGRlZnRhYmxlIGNvaW4tdGFibGU6e2NvaW4tc2NoZW1hfSlcXG5cXG4gIDsgdGhlIHNoYXBlIG9mIGEgY3Jvc3MtY2hhaW4gdHJhbnNmZXIgKHVzZWQgZm9yIHR5cGVjaGVja2luZylcXG4gIChkZWZzY2hlbWEgdHJhbnNmZXItKlxcbiAgICBjcmVhdGUtYWNjb3VudDpzdHJpbmdcXG4gICAgY3JlYXRlLWFjY291bnQtZ3VhcmQ6Z3VhcmRcXG4gICAgcXVhbnRpdHk6ZGVjaW1hbFxcbiAgICApXFxuXFxuICA7IC0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tXFxuICA7IENhcGFiaWxpdGllc1xcblxcbiAgKGRlZmNhcCBHT1ZFUk5BTkNFICgpXFxuICAgIFxcXCJ1cGdyYWRlIGRpc2FibGVkXFxcIlxcbiAgICBmYWxzZSlcXG5cXG4gIChkZWZjYXAgVFJBTlNGRVIgKClcXG4gICAgXFxcIkF1dG9ub21vdXMgY2FwYWJpbGl0eSB0byBwcm90ZWN0IGRlYml0IGFuZCBjcmVkaXQgYWN0aW9uc1xcXCJcXG4gICAgdHJ1ZSlcXG5cXG4gIChkZWZjYXAgQ09JTkJBU0UgKClcXG4gICAgXFxcIk1hZ2ljIGNhcGFiaWxpdHkgdG8gcHJvdGVjdCBtaW5lciByZXdhcmRcXFwiXFxuICAgIHRydWUpXFxuXFxuICAoZGVmY2FwIEZVTkRfVFggKClcXG4gICAgXFxcIk1hZ2ljIGNhcGFiaWxpdHkgdG8gZXhlY3V0ZSBnYXMgcHVyY2hhc2VzIGFuZCByZWRlbXB0aW9uc1xcXCJcXG4gICAgdHJ1ZSlcXG5cXG4gIChkZWZjYXAgQUNDT1VOVF9HVUFSRCAoYWNjb3VudClcXG4gICAgXFxcIkxvb2t1cCBhbmQgZW5mb3JjZSBndWFyZHMgYXNzb2NpYXRlZCB3aXRoIGFuIGFjY291bnRcXFwiXFxuICAgICh3aXRoLXJlYWQgY29pbi10YWJsZSBhY2NvdW50IHsgXFxcImd1YXJkXFxcIiA6PSBnIH1cXG4gICAgICAoZW5mb3JjZS1ndWFyZCBnKSkpXFxuXFxuICAoZGVmY2FwIEdPVkVSTkFOQ0UgKClcXG4gICAgKGVuZm9yY2UgZmFsc2UgXFxcIkVuZm9yY2Ugbm9uLXVwZ3JhZGVhYmlsaXR5IGV4Y2VwdCBpbiB0aGUgY2FzZSBvZiBhIGhhcmQgZm9ya1xcXCIpKVxcblxcbiAgOyAtLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLVxcbiAgOyBDb2luIENvbnRyYWN0XFxuXFxuICAoZGVmdW4gYnV5LWdhczpzdHJpbmcgKHNlbmRlcjpzdHJpbmcgdG90YWw6ZGVjaW1hbClcXG4gICAgQGRvYyBcXFwiVGhpcyBmdW5jdGlvbiBkZXNjcmliZXMgdGhlIG1haW4gJ2dhcyBidXknIG9wZXJhdGlvbi4gQXQgdGhpcyBwb2ludCBcXFxcXFxuICAgIFxcXFxNSU5FUiBoYXMgYmVlbiBjaG9zZW4gZnJvbSB0aGUgcG9vbCwgYW5kIHdpbGwgYmUgdmFsaWRhdGVkLiBUaGUgU0VOREVSICAgXFxcXFxcbiAgICBcXFxcb2YgdGhpcyB0cmFuc2FjdGlvbiBoYXMgc3BlY2lmaWVkIGEgZ2FzIGxpbWl0IExJTUlUIChtYXhpbXVtIGdhcykgZm9yICAgIFxcXFxcXG4gICAgXFxcXHRoZSB0cmFuc2FjdGlvbiwgYW5kIHRoZSBwcmljZSBpcyB0aGUgc3BvdCBwcmljZSBvZiBnYXMgYXQgdGhhdCB0aW1lLiAgICBcXFxcXFxuICAgIFxcXFxUaGUgZ2FzIGJ1eSB3aWxsIGJlIGV4ZWN1dGVkIHByaW9yIHRvIGV4ZWN1dGluZyBTRU5ERVIncyBjb2RlLlxcXCJcXG5cXG4gICAgQG1vZGVsIFsocHJvcGVydHkgKD4gdG90YWwgMC4wKSldXFxuXFxuICAgIChyZXF1aXJlLWNhcGFiaWxpdHkgKEZVTkRfVFgpKVxcbiAgICAod2l0aC1jYXBhYmlsaXR5IChUUkFOU0ZFUilcXG4gICAgICAoZGViaXQgc2VuZGVyIHRvdGFsKSlcXG4gICAgKVxcblxcbiAgKGRlZnVuIHJlZGVlbS1nYXM6c3RyaW5nIChtaW5lcjpzdHJpbmcgbWluZXItZ3VhcmQ6Z3VhcmQgc2VuZGVyOnN0cmluZyB0b3RhbDpkZWNpbWFsKVxcbiAgICBAZG9jIFxcXCJUaGlzIGZ1bmN0aW9uIGRlc2NyaWJlcyB0aGUgbWFpbiAncmVkZWVtIGdhcycgb3BlcmF0aW9uLiBBdCB0aGlzICAgIFxcXFxcXG4gICAgXFxcXHBvaW50LCB0aGUgU0VOREVSJ3MgdHJhbnNhY3Rpb24gaGFzIGJlZW4gZXhlY3V0ZWQsIGFuZCB0aGUgZ2FzIHRoYXQgICAgICBcXFxcXFxuICAgIFxcXFx3YXMgY2hhcmdlZCBoYXMgYmVlbiBjYWxjdWxhdGVkLiBNSU5FUiB3aWxsIGJlIGNyZWRpdGVkIHRoZSBnYXMgY29zdCwgICAgXFxcXFxcbiAgICBcXFxcYW5kIFNFTkRFUiB3aWxsIHJlY2VpdmUgdGhlIHJlbWFpbmRlciB1cCB0byB0aGUgbGltaXRcXFwiXFxuXFxuICAgIEBtb2RlbCBbKHByb3BlcnR5ICg-IHRvdGFsIDAuMCkpXVxcblxcbiAgICAocmVxdWlyZS1jYXBhYmlsaXR5IChGVU5EX1RYKSlcXG4gICAgKHdpdGgtY2FwYWJpbGl0eSAoVFJBTlNGRVIpXFxuICAgICAgKGxldCogKChmZWUgKHJlYWQtZGVjaW1hbCBcXFwiZmVlXFxcIikpXFxuICAgICAgICAgICAgIChyZWZ1bmQgKC0gdG90YWwgZmVlKSkpXFxuICAgICAgICAoZW5mb3JjZSAoPj0gcmVmdW5kIDAuMCkgXFxcImZlZSBtdXN0IGJlIGxlc3MgdGhhbiBvciBlcXVhbCB0byB0b3RhbFxcXCIpXFxuXFxuXFxuICAgICAgICA7IGRpcmVjdGx5IHVwZGF0ZSBpbnN0ZWFkIG9mIGNyZWRpdFxcbiAgICAgICAgKGlmICg-IHJlZnVuZCAwLjApXFxuICAgICAgICAgICh3aXRoLXJlYWQgY29pbi10YWJsZSBzZW5kZXJcXG4gICAgICAgICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6PSBiYWxhbmNlIH1cXG4gICAgICAgICAgICAodXBkYXRlIGNvaW4tdGFibGUgc2VuZGVyXFxuICAgICAgICAgICAgICB7IFxcXCJiYWxhbmNlXFxcIjogKCsgYmFsYW5jZSByZWZ1bmQpIH0pXFxuICAgICAgICAgICAgKVxcbiAgICAgICAgICBcXFwibm9vcFxcXCIpXFxuICAgICAgICAoY3JlZGl0IG1pbmVyIG1pbmVyLWd1YXJkIGZlZSlcXG4gICAgICAgICkpXFxuICAgIClcXG5cXG4gIChkZWZ1biBjcmVhdGUtYWNjb3VudDpzdHJpbmcgKGFjY291bnQ6c3RyaW5nIGd1YXJkOmd1YXJkKVxcbiAgICAoaW5zZXJ0IGNvaW4tdGFibGUgYWNjb3VudFxcbiAgICAgIHsgXFxcImJhbGFuY2VcXFwiIDogMC4wXFxuICAgICAgLCBcXFwiZ3VhcmRcXFwiICAgOiBndWFyZFxcbiAgICAgIH0pXFxuICAgIClcXG5cXG4gIChkZWZ1biBhY2NvdW50LWJhbGFuY2U6ZGVjaW1hbCAoYWNjb3VudDpzdHJpbmcpXFxuICAgICh3aXRoLXJlYWQgY29pbi10YWJsZSBhY2NvdW50XFxuICAgICAgeyBcXFwiYmFsYW5jZVxcXCIgOj0gYmFsYW5jZSB9XFxuICAgICAgYmFsYW5jZVxcbiAgICAgIClcXG4gICAgKVxcblxcbiAgKGRlZnVuIHRyYW5zZmVyOnN0cmluZyAoc2VuZGVyOnN0cmluZyByZWNlaXZlcjpzdHJpbmcgcmVjZWl2ZXItZ3VhcmQ6Z3VhcmQgYW1vdW50OmRlY2ltYWwpXFxuICAgICh3aXRoLWNhcGFiaWxpdHkgKFRSQU5TRkVSKVxcbiAgICAgIChkZWJpdCBzZW5kZXIgYW1vdW50KVxcbiAgICAgIChjcmVkaXQgcmVjZWl2ZXIgcmVjZWl2ZXItZ3VhcmQgYW1vdW50KSlcXG4gICAgKVxcblxcbiAgKGRlZnVuIGNvaW5iYXNlOnN0cmluZyAoYWRkcmVzczpzdHJpbmcgYWRkcmVzcy1ndWFyZDpndWFyZCBhbW91bnQ6ZGVjaW1hbClcXG4gICAgKHJlcXVpcmUtY2FwYWJpbGl0eSAoQ09JTkJBU0UpKVxcbiAgICAod2l0aC1jYXBhYmlsaXR5IChUUkFOU0ZFUilcXG4gICAgIChjcmVkaXQgYWRkcmVzcyBhZGRyZXNzLWd1YXJkIGFtb3VudCkpXFxuICAgIClcXG5cXG4gIChkZWZwYWN0IGZ1bmQtdHggKHNlbmRlciBtaW5lciBtaW5lci1ndWFyZCB0b3RhbClcXG4gICAgQGRvYyBcXFwiJ2Z1bmQtdHgnIGlzIGEgc3BlY2lhbCBwYWN0IHRvIGZ1bmQgYSB0cmFuc2FjdGlvbiBpbiB0d28gc3RlcHMsICAgICBcXFxcXFxuICAgIFxcXFx3aXRoIHRoZSBhY3R1YWwgdHJhbnNhY3Rpb24gdHJhbnNwaXJpbmcgaW4gdGhlIG1pZGRsZTogICAgICAgICAgICAgICAgICAgXFxcXFxcbiAgICBcXFxcICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFxcXFxcXG4gICAgXFxcXCAgMSkgQSBidXlpbmcgcGhhc2UsIGRlYml0aW5nIHRoZSBzZW5kZXIgZm9yIHRvdGFsIGdhcyBhbmQgZmVlLCB5aWVsZGluZyBcXFxcXFxuICAgIFxcXFwgICAgIFRYX01BWF9DSEFSR0UuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXFxcXFxcbiAgICBcXFxcICAyKSBBIHNldHRsZW1lbnQgcGhhc2UsIHJlc3VtaW5nIFRYX01BWF9DSEFSR0UsIGFuZCBhbGxvY2F0aW5nIHRvIHRoZSAgIFxcXFxcXG4gICAgXFxcXCAgICAgY29pbmJhc2UgYWNjb3VudCBmb3IgdXNlZCBnYXMgYW5kIGZlZSwgYW5kIHNlbmRlciBhY2NvdW50IGZvciBiYWwtICBcXFxcXFxuICAgIFxcXFwgICAgIGFuY2UgKHVudXNlZCBnYXMsIGlmIGFueSkuXFxcIlxcblxcbiAgICAoc3RlcCAoYnV5LWdhcyBzZW5kZXIgdG90YWwpKVxcbiAgICAoc3RlcCAocmVkZWVtLWdhcyBtaW5lciBtaW5lci1ndWFyZCBzZW5kZXIgdG90YWwpKVxcbiAgICApXFxuXFxuICAoZGVmdW4gZGViaXQ6c3RyaW5nIChhY2NvdW50OnN0cmluZyBhbW91bnQ6ZGVjaW1hbClcXG4gICAgQGRvYyBcXFwiRGViaXQgQU1PVU5UIGZyb20gQUNDT1VOVCBiYWxhbmNlIHJlY29yZGluZyBEQVRFIGFuZCBEQVRBXFxcIlxcblxcbiAgICBAbW9kZWwgWyhwcm9wZXJ0eSAoPiBhbW91bnQgMC4wKSldXFxuXFxuICAgIChyZXF1aXJlLWNhcGFiaWxpdHkgKFRSQU5TRkVSKSlcXG4gICAgKHdpdGgtY2FwYWJpbGl0eSAoQUNDT1VOVF9HVUFSRCBhY2NvdW50KVxcbiAgICAgICh3aXRoLXJlYWQgY29pbi10YWJsZSBhY2NvdW50XFxuICAgICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6PSBiYWxhbmNlIH1cXG5cXG4gICAgICAgIChlbmZvcmNlICg8PSBhbW91bnQgYmFsYW5jZSkgXFxcIkluc3VmZmljaWVudCBmdW5kc1xcXCIpXFxuICAgICAgICAodXBkYXRlIGNvaW4tdGFibGUgYWNjb3VudFxcbiAgICAgICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6ICgtIGJhbGFuY2UgYW1vdW50KSB9XFxuICAgICAgICAgICkpKVxcbiAgICApXFxuXFxuXFxuICAoZGVmdW4gY3JlZGl0OnN0cmluZyAoYWNjb3VudDpzdHJpbmcgZ3VhcmQ6Z3VhcmQgYW1vdW50OmRlY2ltYWwpXFxuICAgIEBkb2MgXFxcIkNyZWRpdCBBTU9VTlQgdG8gQUNDT1VOVCBiYWxhbmNlIHJlY29yZGluZyBEQVRFIGFuZCBEQVRBXFxcIlxcblxcbiAgICBAbW9kZWwgWyhwcm9wZXJ0eSAoPiBhbW91bnQgMC4wKSldXFxuXFxuICAgIChyZXF1aXJlLWNhcGFiaWxpdHkgKFRSQU5TRkVSKSlcXG4gICAgKHdpdGgtZGVmYXVsdC1yZWFkIGNvaW4tdGFibGUgYWNjb3VudFxcbiAgICAgIHsgXFxcImJhbGFuY2VcXFwiIDogMC4wLCBcXFwiZ3VhcmRcXFwiIDogZ3VhcmQgfVxcbiAgICAgIHsgXFxcImJhbGFuY2VcXFwiIDo9IGJhbGFuY2UsIFxcXCJndWFyZFxcXCIgOj0gcmV0ZyB9XFxuICAgICAgOyB3ZSBkb24ndCB3YW50IHRvIG92ZXJ3cml0ZSBhbiBleGlzdGluZyBndWFyZCB3aXRoIHRoZSB1c2VyLXN1cHBsaWVkIG9uZVxcbiAgICAgIChlbmZvcmNlICg9IHJldGcgZ3VhcmQpXFxuICAgICAgICBcXFwiYWNjb3VudCBndWFyZHMgZG8gbm90IG1hdGNoXFxcIilcXG5cXG4gICAgICAod3JpdGUgY29pbi10YWJsZSBhY2NvdW50XFxuICAgICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6ICgrIGJhbGFuY2UgYW1vdW50KVxcbiAgICAgICAgLCBcXFwiZ3VhcmRcXFwiICAgOiByZXRnXFxuICAgICAgICB9KVxcbiAgICAgICkpXFxuXFxuICAoZGVmcGFjdCBjcm9zcy1jaGFpbi10cmFuc2ZlclxcbiAgICAoIGRlbGV0ZS1hY2NvdW50OnN0cmluZ1xcbiAgICAgIGNyZWF0ZS1jaGFpbi1pZDpzdHJpbmdcXG4gICAgICBjcmVhdGUtYWNjb3VudDpzdHJpbmdcXG4gICAgICBjcmVhdGUtYWNjb3VudC1ndWFyZDpndWFyZFxcbiAgICAgIHF1YW50aXR5OmRlY2ltYWwgKVxcblxcbiAgICBAZG9jIFxcXCJTdGVwIDE6IEJ1cm4gUVVBTlRJVFktbWFueSBjb2lucyBmb3IgREVMRVRFLUFDQ09VTlQgb24gdGhlIGN1cnJlbnQgY2hhaW4sIFxcXFxcXG4gICAgICAgICBcXFxcYW5kIHByb2R1Y2UgYW4gU1BWIHJlY2VpcHQgd2hpY2ggbWF5IGJlIG1hbnVhbGx5IHJlZGVlbWVkIGZvciBhbiBTUFYgICAgICBcXFxcXFxuICAgICAgICAgXFxcXHByb29mLiBPbmNlIGEgcHJvb2YgaXMgb2J0YWluZWQsIHRoZSB1c2VyIG1heSBjYWxsICdjcmVhdGUtY29pbicgYW5kICAgICAgXFxcXFxcbiAgICAgICAgIFxcXFxjb25zdW1lIHRoZSBwcm9vZiBvbiBDUkVBVEUtQ0hBSU4tSUQsIGNyZWRpdGluZyBDUkVBVEUtQUNDT1VOVCBRVUFOVElUWS0gIFxcXFxcXG4gICAgICAgICBcXFxcbWFueSBjb2lucy4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBcXFxcXFxuICAgICAgICAgXFxcXCAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXFxcXFxcbiAgICAgICAgIFxcXFxTdGVwIDI6IENvbnN1bWUgYW4gU1BWIHByb29mIGZvciBhIG51bWJlciBvZiBjb2lucywgYW5kIGNyZWRpdCB0aGUgICAgICAgIFxcXFxcXG4gICAgICAgICBcXFxcYWNjb3VudCBhc3NvY2lhdGVkIHdpdGggdGhlIHByb29mIHRoZSBxdWFudGlmeSBvZiBjb2lucyBidXJuZWQgb24gdGhlICAgICBcXFxcXFxuICAgICAgICAgXFxcXHNvdXJjZSBjaGFpbiBieSB0aGUgYnVybiBhY2NvdW50LiBOb3RlOiBtdXN0IGJlIGNhbGxlZCBvbiB0aGUgY29ycmVjdCAgICAgXFxcXFxcbiAgICAgICAgIFxcXFxjaGFpbiBpZCBhcyBzcGVjaWZpZWQgaW4gdGhlIHByb29mLlxcXCJcXG5cXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5ICg-IHF1YW50aXR5IDAuMCkpXFxuICAgICAgICAgICAsIChwcm9wZXJ0eSAobm90ICg9IGNyZWF0ZS1jaGFpbi1pZCBcXFwiXFxcIikpKVxcbiAgICAgICAgICAgXVxcblxcbiAgICAoc3RlcFxcbiAgICAgICh3aXRoLWNhcGFiaWxpdHkgKFRSQU5TRkVSKVxcbiAgICAgICAgKGRlYml0IGRlbGV0ZS1hY2NvdW50IHF1YW50aXR5KVxcbiAgICAgICAgICAobGV0XFxuICAgICAgICAgICAgICAoKHJldHY6b2JqZWN0e3RyYW5zZmVyLSp9XFxuICAgICAgICAgICAgICAgICAgeyBcXFwiY3JlYXRlLWFjY291bnRcXFwiOiBjcmVhdGUtYWNjb3VudFxcbiAgICAgICAgICAgICAgICAgICwgXFxcImNyZWF0ZS1hY2NvdW50LWd1YXJkXFxcIjogY3JlYXRlLWFjY291bnQtZ3VhcmRcXG4gICAgICAgICAgICAgICAgICAsIFxcXCJxdWFudGl0eVxcXCI6IHF1YW50aXR5XFxuICAgICAgICAgICAgICAgICAgfSkpXFxuICAgICAgICAgICAgKHlpZWxkIHJldHYgY3JlYXRlLWNoYWluLWlkKVxcbiAgICAgICAgICAgICkpKVxcblxcbiAgICAoc3RlcFxcbiAgICAgIChyZXN1bWVcXG4gICAgICAgIHsgXFxcImNyZWF0ZS1hY2NvdW50XFxcIiA6PSBjcmVhdGUtYWNjb3VudFxcbiAgICAgICAgLCBcXFwiY3JlYXRlLWFjY291bnQtZ3VhcmRcXFwiIDo9IGNyZWF0ZS1hY2NvdW50LWd1YXJkXFxuICAgICAgICAsIFxcXCJxdWFudGl0eVxcXCIgOj0gcXVhbnRpdHlcXG4gICAgICAgIH1cXG5cXG4gICAgICAgICh3aXRoLWNhcGFiaWxpdHkgKFRSQU5TRkVSKVxcbiAgICAgICAgICAoY3JlZGl0IGNyZWF0ZS1hY2NvdW50IGNyZWF0ZS1hY2NvdW50LWd1YXJkIHF1YW50aXR5KSlcXG4gICAgICAgICkpXFxuICAgIClcXG4pXFxuXFxuKGNyZWF0ZS10YWJsZSBjb2luLXRhYmxlKVxcblwifX0sXCJzaWduZXJzXCI6W10sXCJtZXRhXCI6e1wiZ2FzTGltaXRcIjowLFwiY2hhaW5JZFwiOlwiXCIsXCJnYXNQcmljZVwiOjAsXCJzZW5kZXJcIjpcIlwifSxcIm5vbmNlXCI6XCJcXFwiZ2VuZXNpcy0wMVxcXCJcIn0ifQ
  - eyJnYXMiOjAsInJlc3VsdCI6eyJzdGF0dXMiOiJzdWNjZXNzIiwiZGF0YSI6IlRhYmxlQ3JlYXRlZCJ9LCJyZXFLZXkiOiJ4TUhPOGxBUFh2V1V2SFViTUVPb0kzYmkwcU5JNXczRVRDVWk1alNmZk9NIiwibG9ncyI6Il9tLS1GUEUtNV9Tc19vRm5FcHBTWmhfOTFoQjhfNVhUUDh5aUhVWnhCcUEiLCJtZXRhRGF0YSI6bnVsbCwiY29udGludWF0aW9uIjpudWxsLCJ0eElkIjoxfQ
- - eyJoYXNoIjoiUEoyb1NsaEJBVUtnU1ViaFFxY2RFNVBRa1lCNDkzSlVwZnNLYU54cmhjayIsInNpZ3MiOltdLCJjbWQiOiJ7XCJwYXlsb2FkXCI6e1wiZXhlY1wiOntcImRhdGFcIjp7XCJzZW5kZXIwN1wiOltcIjRjMzFkYzllZTdmMjQxNzdmNzhiNmY1MTgwMTJhMjA4MzI2ZTJhZjFmMzdiYjBhMjQwNWI1MDU2ZDBjYWQ2MjhcIl0sXCJzZW5kZXIwMVwiOltcIjZiZTJmNDg1YTdhZjc1ZmVkYjRiN2YxNTNhOTAzZjdlNjAwMGNhNGFhNTAxMTc5YzkxYTI0NTBiNzc3YmQyYTdcIl0sXCJzZW5kZXIwNlwiOltcIjVmZmMxZjdmZWY3YTQ0NzM4NjI1NzYyZjc1YTQyMjk0NTQ5NTFlMDNmMmFmYzZmODEzMDljMGMxYmRmOWVlNmZcIl0sXCJzZW5kZXIwMFwiOltcIjM2ODgyMGY4MGMzMjRiYmM3YzJiMDYxMDY4OGE3ZGE0M2UzOWY5MWQxMTg3MzI2NzFjZDljNzUwMGZmNDNjY2FcIl0sXCJzZW5kZXIwNVwiOltcImYwOWQ4ZjYzOTRhZWE0MjVmZTY3ODNkODhjZDgxMzYzZDgwMTdmMTZhZmQzNzExYzU3NWJlMGY1Y2Q1YzliYjlcIl0sXCJzZW5kZXIwNFwiOltcIjJkNzBhYTRmNjk3YzNhM2I4ZGQ2ZDk3NzQ1YWMwNzRlZGNmZDBlYjY1YzM3Nzc0Y2RlMjUxMzU0ODNiZWE3MWVcIl0sXCJtdWx0aS0wMi0wMy0wNC1hbnlcIjp7XCJwcmVkXCI6XCJrZXlzLWFueVwiLFwia2V5c1wiOltcIjNhOWRkNTMyZDczZGFjZTE5NWRiYjY0ZDFkYmE2NTcyZmI3ODNkMGZkZDMyNDY4NWUzMmZiZGEyZjg5Zjk5YTZcIixcIjQzZjJhZGIxZGUxOTIwMDBjYjM3NzdiYWNjN2Y5ODNiNjYxNGZkOWMxNzE1Y2Q0NGNkNDg0YjZkM2EwZDM0YzhcIixcIjJkNzBhYTRmNjk3YzNhM2I4ZGQ2ZDk3NzQ1YWMwNzRlZGNmZDBlYjY1YzM3Nzc0Y2RlMjUxMzU0ODNiZWE3MWVcIl19LFwic2VuZGVyMDlcIjpbXCJjNTlkOTg0MGIwYjY2MDkwODM2NTQ2YjdlYjRhNzM2MDYyNTc1MjdlYzhjMmI0ODIzMDBmZDIyOTI2NGIwN2U2XCJdLFwic2VuZGVyMDNcIjpbXCI0M2YyYWRiMWRlMTkyMDAwY2IzNzc3YmFjYzdmOTgzYjY2MTRmZDljMTcxNWNkNDRjZDQ4NGI2ZDNhMGQzNGM4XCJdLFwibXVsdGktMDAtMDFcIjpbXCIzNjg4MjBmODBjMzI0YmJjN2MyYjA2MTA2ODhhN2RhNDNlMzlmOTFkMTE4NzMyNjcxY2Q5Yzc1MDBmZjQzY2NhXCIsXCI2YmUyZjQ4NWE3YWY3NWZlZGI0YjdmMTUzYTkwM2Y3ZTYwMDBjYTRhYTUwMTE3OWM5MWEyNDUwYjc3N2JkMmE3XCJdLFwic2VuZGVyMDhcIjpbXCI2M2IyZWJhNGVkNzBkNDYxMmQzZTdiYzkwZGIyZmJmNGM3NmY3YjA3NDM2M2U4NmQ3M2YwYmM2MTdmOGU4YjgxXCJdLFwic2VuZGVyMDJcIjpbXCIzYTlkZDUzMmQ3M2RhY2UxOTVkYmI2NGQxZGJhNjU3MmZiNzgzZDBmZGQzMjQ2ODVlMzJmYmRhMmY4OWY5OWE2XCJdfSxcImNvZGVcIjpcIihjb2luLmNvaW5iYXNlIFxcXCJzZW5kZXIwMFxcXCIgKHJlYWQta2V5c2V0IFxcXCJzZW5kZXIwMFxcXCIpIDEwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDFcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDFcXFwiKSAxMDEwLjApXFxuKGNvaW4uY29pbmJhc2UgXFxcInNlbmRlcjAyXFxcIiAocmVhZC1rZXlzZXQgXFxcInNlbmRlcjAyXFxcIikgMTAyMC4wKVxcbihjb2luLmNvaW5iYXNlIFxcXCJzZW5kZXIwM1xcXCIgKHJlYWQta2V5c2V0IFxcXCJzZW5kZXIwM1xcXCIpIDEwMzAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDRcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDRcXFwiKSAxMDQwLjApXFxuKGNvaW4uY29pbmJhc2UgXFxcInNlbmRlcjA1XFxcIiAocmVhZC1rZXlzZXQgXFxcInNlbmRlcjA1XFxcIikgMTA1MC4wKVxcbihjb2luLmNvaW5iYXNlIFxcXCJzZW5kZXIwNlxcXCIgKHJlYWQta2V5c2V0IFxcXCJzZW5kZXIwNlxcXCIpIDEwNjAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDdcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDdcXFwiKSAxMDcwLjApXFxuKGNvaW4uY29pbmJhc2UgXFxcInNlbmRlcjA4XFxcIiAocmVhZC1rZXlzZXQgXFxcInNlbmRlcjA4XFxcIikgMTA4MC4wKVxcbihjb2luLmNvaW5iYXNlIFxcXCJzZW5kZXIwOVxcXCIgKHJlYWQta2V5c2V0IFxcXCJzZW5kZXIwOVxcXCIpIDEwOTAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwibXVsdGktMDAtMDFcXFwiIChyZWFkLWtleXNldCBcXFwibXVsdGktMDAtMDFcXFwiKSAxMDAxLjApXFxuKGNvaW4uY29pbmJhc2UgXFxcIm11bHRpLTAyLTAzLTA0LWFueVxcXCIgKHJlYWQta2V5c2V0IFxcXCJtdWx0aS0wMi0wMy0wNC1hbnlcXFwiKSAxMjM0LjApXCJ9fSxcInNpZ25lcnNcIjpbXSxcIm1ldGFcIjp7XCJnYXNMaW1pdFwiOjAsXCJjaGFpbklkXCI6XCJcIixcImdhc1ByaWNlXCI6MCxcInNlbmRlclwiOlwiXCJ9LFwibm9uY2VcIjpcIlxcXCJ0ZXN0bmV0MDAtZ3JhbnRzXFxcIlwifSJ9
  - eyJnYXMiOjAsInJlc3VsdCI6eyJzdGF0dXMiOiJzdWNjZXNzIiwiZGF0YSI6IldyaXRlIHN1Y2NlZWRlZCJ9LCJyZXFLZXkiOiJQSjJvU2xoQkFVS2dTVWJoUXFjZEU1UFFrWUI0OTNKVXBmc0thTnhyaGNrIiwibG9ncyI6Ilp5aHdyRlMyYnJicHZwZ0daTnBxSE5FcEt3X2dRRk5SVEE4RDk3QjQ2NE0iLCJtZXRhRGF0YSI6bnVsbCwiY29udGludWF0aW9uIjpudWxsLCJ0eElkIjoyfQ
minerData: eyJtIjoiTm9NaW5lciIsImtzIjpbXSwia3AiOiI8In0
transactionsHash: ZbVpSdVrd3_jasbaUMsaxu1AM_6w5OeEXylq5Thaepk
outputsHash: fyOjEGCAaly-n4wduuZBsit5UnnhWKn5PY4HYDXo2EU
payloadHash: 6F8qw_b1eNyZH58k2XWkPREjoH1hgrZ2gYcKpqngwSw
coinbase: eyJnYXMiOjAsInJlc3VsdCI6eyJzdGF0dXMiOiJzdWNjZXNzIiwiZGF0YSI6Ik5PX0NPSU5CQVNFIn0sInJlcUtleSI6IkRsZFJ3Q2JsUTdMb3F5NndZSm5hb2RIbDMwZDNqM2VILXF0RnpmRXY0NmciLCJsb2dzIjpudWxsLCJtZXRhRGF0YSI6bnVsbCwiY29udGludWF0aW9uIjpudWxsLCJ0eElkIjpudWxsfQ

|]
