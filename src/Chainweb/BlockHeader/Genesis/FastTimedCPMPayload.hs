{-# LANGUAGE QuasiQuotes #-}

-- This module is auto-generated. DO NOT EDIT IT MANUALLY.

module Chainweb.BlockHeader.Genesis.FastTimedCPMPayload ( payloadBlock ) where

import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (decodeThrow)

import NeatInterpolation (text)

import Chainweb.Payload (PayloadWithOutputs)
import Chainweb.Utils (fromJuste)

payloadBlock :: PayloadWithOutputs
payloadBlock = fromJuste $ decodeThrow $ encodeUtf8 [text|
transactions:
- - eyJoYXNoIjoiN0dFZGdTV3Bid2ZwZk91OUlSYnU5N2RhMEk2Vng2U2VCWENsWGY0TC1MNCIsInNpZ3MiOltdLCJjbWQiOiJ7XCJwYXlsb2FkXCI6e1wiZXhlY1wiOntcImRhdGFcIjpudWxsLFwiY29kZVwiOlwiKGludGVyZmFjZSBjb2luLXNpZ1xcblxcbiAgXFxcIidjb2luLXNpZycgcmVwcmVzZW50cyB0aGUgS2FkZW5hIENvaW4gQ29udHJhY3QgaW50ZXJmYWNlLiBUaGlzIGNvbnRyYWN0ICAgICBcXFxcXFxuICBcXFxccHJvdmlkZXMgYm90aCB0aGUgdGhlIGdlbmVyYWwgaW50ZXJmYWNlIGZvciBhIEthZGVuYSdzIHRva2VuLCBzdXBwbHlpbmcgYSAgIFxcXFxcXG4gIFxcXFx0cmFuc2ZlciBmdW5jdGlvbiwgY29pbmJhc2UsIGFjY291bnQgY3JlYXRpb24gYW5kIGJhbGFuY2UgcXVlcnkuXFxcIlxcblxcbiAgKGRlZnVuIGNyZWF0ZS1hY2NvdW50OnN0cmluZyAoYWNjb3VudDpzdHJpbmcgZ3VhcmQ6Z3VhcmQpXFxuICAgIEBkb2MgXFxcIkNyZWF0ZSBhbiBhY2NvdW50IGZvciBBQ0NPVU5ULCB3aXRoIEdVQVJEIGNvbnRyb2xsaW5nIGFjY2VzcyB0byB0aGUgIFxcXFxcXG4gICAgXFxcXGFjY291bnQuXFxcIlxcbiAgICBAbW9kZWwgWyAocHJvcGVydHkgKG5vdCAoPSBhY2NvdW50IFxcXCJcXFwiKSkpIF1cXG4gICAgKVxcblxcbiAgKGRlZnVuIHRyYW5zZmVyOnN0cmluZyAoc2VuZGVyOnN0cmluZyByZWNlaXZlcjpzdHJpbmcgYW1vdW50OmRlY2ltYWwpXFxuICAgIEBkb2MgXFxcIlRyYW5zZmVyIEFNT1VOVCBiZXR3ZWVuIGFjY291bnRzIFNFTkRFUiBhbmQgUkVDRUlWRVIgb24gdGhlIHNhbWUgICAgXFxcXFxcbiAgICBcXFxcY2hhaW4uIFRoaXMgZmFpbHMgaWYgZWl0aGVyIFNFTkRFUiBvciBSRUNFSVZFUiBkb2VzIG5vdCBleGlzdC4gICAgICAgICAgIFxcXFxcXG4gICAgXFxcXENyZWF0ZS1vbi10cmFuc2ZlciBjYW4gYmUgZG9uZSB1c2luZyB0aGUgJ3RyYW5zZmVyLWFuZC1jcmVhdGUnIGZ1bmN0aW9uLlxcXCJcXG5cXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5ICg-IGFtb3VudCAwLjApKVxcbiAgICAgICAgICAgICAocHJvcGVydHkgKG5vdCAoPSBzZW5kZXIgcmVjZWl2ZXIpKSlcXG4gICAgICAgICAgIF1cXG4gICAgKVxcblxcbiAgKGRlZnVuIHRyYW5zZmVyLWFuZC1jcmVhdGU6c3RyaW5nXFxuICAgICggc2VuZGVyOnN0cmluZ1xcbiAgICAgIHJlY2VpdmVyOnN0cmluZ1xcbiAgICAgIHJlY2VpdmVyLWd1YXJkOmd1YXJkXFxuICAgICAgYW1vdW50OmRlY2ltYWwgKVxcblxcbiAgICBAZG9jIFxcXCJUcmFuc2ZlciBBTU9VTlQgYmV0d2VlbiBhY2NvdW50cyBTRU5ERVIgYW5kIFJFQ0VJVkVSIG9uIHRoZSBzYW1lICAgIFxcXFxcXG4gICAgXFxcXGNoYWluLiBUaGlzIGZhaWxzIGlmIFNFTkRFUiBkb2VzIG5vdCBleGlzdC4gSWYgdGhlIFJFQ0VJVkVSIGFjY291bnQgZG9lcyBcXFxcXFxuICAgIFxcXFxub3QgZXhpc3QsIHRoZW4gaXQgaXMgY3JlYXRlZCBhbmQgYXNzb2NpYXRlZCB3aXRoIFJFQ0VJVkVSLUdVQVJELlxcXCJcXG5cXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5ICg-IGFtb3VudCAwLjApKVxcbiAgICAgICAgICAgICAocHJvcGVydHkgKG5vdCAoPSBzZW5kZXIgcmVjZWl2ZXIpKSlcXG4gICAgICAgICAgIF1cXG4gICAgKVxcblxcbiAgKGRlZnVuIGFjY291bnQtYmFsYW5jZTpkZWNpbWFsIChhY2NvdW50OnN0cmluZylcXG4gICAgQGRvYyBcXFwiQ2hlY2sgYW4gYWNjb3VudCdzIGJhbGFuY2VcXFwiXFxuICAgIEBtb2RlbCBbIChwcm9wZXJ0eSAobm90ICg9IGFjY291bnQgXFxcIlxcXCIpKSkgXVxcbiAgICApXFxuXFxuICAoZGVmdW4gYWNjb3VudC1pbmZvOm9iamVjdCAoYWNjb3VudDpzdHJpbmcpXFxuICAgIEBkb2MgXFxcIkdldCBhbGwgb2YgYW4gYWNjb3VudCdzIGluZm8uIFRoaXMgaW5jbHVkZXMgdGhlIGJhbGFuY2UgYW5kIHRoZSAgICAgXFxcXFxcbiAgICBcXFxcZ3VhcmQuXFxcIlxcbiAgICBAbW9kZWwgWyAocHJvcGVydHkgKG5vdCAoPSBhY2NvdW50IFxcXCJcXFwiKSkpIF0pXFxuXFxuICAoZGVmdW4gcm90YXRlLWFjY291bnQtZ3VhcmQ6c3RyaW5nIChhY2NvdW50OnN0cmluZyBuZXctZ3VhcmQ6Z3VhcmQpXFxuICAgIEBkb2MgXFxcIlJvdGF0ZSBndWFyZCBhc3NvY2lhdGVkIHdpdGggQUNDT1VOVCB0byBuZXcgZ3VhcmQgTkVXLUdVQVJEXFxcIlxcbiAgICBAbW9kZWwgWyAocHJvcGVydHkgKG5vdCAoPSBhY2NvdW50IFxcXCJcXFwiKSkpIF1cXG4gICAgKVxcblxcbiAgKGRlZnVuIGNvaW5iYXNlOnN0cmluZyAoYWRkcmVzczpzdHJpbmcgYWRkcmVzcy1ndWFyZDpndWFyZCBhbW91bnQ6ZGVjaW1hbClcXG4gICAgQGRvYyBcXFwiTWludCBzb21lIG51bWJlciBvZiB0b2tlbnMgYW5kIGFsbG9jYXRlIHRoZW0gdG8gc29tZSBhZGRyZXNzXFxcIlxcblxcbiAgICBAbW9kZWwgWyAocHJvcGVydHkgKD4gYW1vdW50IDAuMCkpXFxuICAgICAgICAgICAgIChwcm9wZXJ0eSAobm90ICg9IGFkZHJlc3MgXFxcIlxcXCIpKSlcXG4gICAgICAgICAgIF1cXG4gICAgKVxcblxcbilcXG5cIn19LFwic2lnbmVyc1wiOltdLFwibWV0YVwiOntcImNyZWF0aW9uVGltZVwiOjAsXCJ0dGxcIjowLFwiZ2FzTGltaXRcIjowLFwiY2hhaW5JZFwiOlwiXCIsXCJnYXNQcmljZVwiOjAsXCJzZW5kZXJcIjpcIlwifSxcIm5vbmNlXCI6XCJcXFwiZ2VuZXNpcy0wMVxcXCJcIn0ifQ
  - eyJnYXMiOjAsInJlc3VsdCI6eyJzdGF0dXMiOiJzdWNjZXNzIiwiZGF0YSI6IldyaXRlIHN1Y2NlZWRlZCJ9LCJyZXFLZXkiOiJiV3RLU0g2NzFPUFN2VGtmTzZSR0c1RGFSLTdIMncxVHpxcDZzeWF3bWlrIiwibG9ncyI6ImVWamN4QndjUnQ4M25COUM0RzFJVUdBVnVqOHdpd0pvM0tJLUI4b3ZEbVEiLCJtZXRhRGF0YSI6bnVsbCwiY29udGludWF0aW9uIjpudWxsLCJ0eElkIjoyfQ
- - eyJoYXNoIjoiZklqZEZmTlFxaF9SZy1sV2ZjS19aOFAtSjFNMnhxNUY5X3ZGS3FJWk1CYyIsInNpZ3MiOltdLCJjbWQiOiJ7XCJwYXlsb2FkXCI6e1wiZXhlY1wiOntcImRhdGFcIjpudWxsLFwiY29kZVwiOlwiKG1vZHVsZSBjb2luIEdPVkVSTkFOQ0VcXG5cXG4gIFxcXCInY29pbicgcmVwcmVzZW50cyB0aGUgS2FkZW5hIENvaW4gQ29udHJhY3QuIFRoaXMgY29udHJhY3QgcHJvdmlkZXMgYm90aCB0aGUgXFxcXFxcbiAgXFxcXGJ1eS9yZWRlZW0gZ2FzIHN1cHBvcnQgaW4gdGhlIGZvcm0gb2YgJ2Z1bmQtdHgnLCBhcyB3ZWxsIGFzIHRyYW5zZmVyLCAgICAgICBcXFxcXFxuICBcXFxcY3JlZGl0LCBkZWJpdCwgY29pbmJhc2UsIGFjY291bnQgY3JlYXRpb24gYW5kIHF1ZXJ5LCBhcyB3ZWxsIGFzIFNQViBidXJuICAgIFxcXFxcXG4gIFxcXFxjcmVhdGUuIFRvIGFjY2VzcyB0aGUgY29pbiBjb250cmFjdCwgeW91IG1heSB1c2UgaXRzIGZ1bGx5LXF1YWxpZmllZCBuYW1lLCAgXFxcXFxcbiAgXFxcXG9yIGlzc3VlIHRoZSAnKHVzZSBjb2luKScgY29tbWFuZCBpbiB0aGUgYm9keSBvZiBhIG1vZHVsZSBkZWNsYXJhdGlvbi5cXFwiXFxuXFxuXFxuICAoaW1wbGVtZW50cyBjb2luLXNpZylcXG5cXG4gIDsgLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS1cXG4gIDsgU2NoZW1hcyBhbmQgVGFibGVzXFxuXFxuICAoZGVmc2NoZW1hIGNvaW4tc2NoZW1hXFxuICAgIGJhbGFuY2U6ZGVjaW1hbFxcbiAgICBndWFyZDpndWFyZClcXG4gIChkZWZ0YWJsZSBjb2luLXRhYmxlOntjb2luLXNjaGVtYX0pXFxuXFxuICA7IHRoZSBzaGFwZSBvZiBhIGNyb3NzLWNoYWluIHRyYW5zZmVyICh1c2VkIGZvciB0eXBlY2hlY2tpbmcpXFxuICAoZGVmc2NoZW1hIHRyYW5zZmVyLXNjaGVtYVxcbiAgICBjcmVhdGUtYWNjb3VudDpzdHJpbmdcXG4gICAgY3JlYXRlLWFjY291bnQtZ3VhcmQ6Z3VhcmRcXG4gICAgcXVhbnRpdHk6ZGVjaW1hbFxcbiAgICApXFxuXFxuICA7IC0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tXFxuICA7IENhcGFiaWxpdGllc1xcblxcbiAgKGRlZmNhcCBUUkFOU0ZFUiAoKVxcbiAgICBcXFwiQXV0b25vbW91cyBjYXBhYmlsaXR5IHRvIHByb3RlY3QgZGViaXQgYW5kIGNyZWRpdCBhY3Rpb25zXFxcIlxcbiAgICB0cnVlKVxcblxcbiAgKGRlZmNhcCBDT0lOQkFTRSAoKVxcbiAgICBcXFwiTWFnaWMgY2FwYWJpbGl0eSB0byBwcm90ZWN0IG1pbmVyIHJld2FyZFxcXCJcXG4gICAgdHJ1ZSlcXG5cXG4gIChkZWZjYXAgRlVORF9UWCAoKVxcbiAgICBcXFwiTWFnaWMgY2FwYWJpbGl0eSB0byBleGVjdXRlIGdhcyBwdXJjaGFzZXMgYW5kIHJlZGVtcHRpb25zXFxcIlxcbiAgICB0cnVlKVxcblxcbiAgKGRlZmNhcCBBQ0NPVU5UX0dVQVJEIChhY2NvdW50KVxcbiAgICBcXFwiTG9va3VwIGFuZCBlbmZvcmNlIGd1YXJkcyBhc3NvY2lhdGVkIHdpdGggYW4gYWNjb3VudFxcXCJcXG4gICAgKHdpdGgtcmVhZCBjb2luLXRhYmxlIGFjY291bnQgeyBcXFwiZ3VhcmRcXFwiIDo9IGcgfVxcbiAgICAgIChlbmZvcmNlLWd1YXJkIGcpKSlcXG5cXG4gIChkZWZjYXAgR09WRVJOQU5DRSAoKVxcbiAgICAoZW5mb3JjZSBmYWxzZSBcXFwiRW5mb3JjZSBub24tdXBncmFkZWFiaWxpdHkgZXhjZXB0IGluIHRoZSBjYXNlIG9mIGEgaGFyZCBmb3JrXFxcIikpXFxuXFxuICA7IC0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tXFxuICA7IENvaW4gQ29udHJhY3RcXG5cXG4gIChkZWZ1biBidXktZ2FzOnN0cmluZyAoc2VuZGVyOnN0cmluZyB0b3RhbDpkZWNpbWFsKVxcbiAgICBAZG9jIFxcXCJUaGlzIGZ1bmN0aW9uIGRlc2NyaWJlcyB0aGUgbWFpbiAnZ2FzIGJ1eScgb3BlcmF0aW9uLiBBdCB0aGlzIHBvaW50IFxcXFxcXG4gICAgXFxcXE1JTkVSIGhhcyBiZWVuIGNob3NlbiBmcm9tIHRoZSBwb29sLCBhbmQgd2lsbCBiZSB2YWxpZGF0ZWQuIFRoZSBTRU5ERVIgICBcXFxcXFxuICAgIFxcXFxvZiB0aGlzIHRyYW5zYWN0aW9uIGhhcyBzcGVjaWZpZWQgYSBnYXMgbGltaXQgTElNSVQgKG1heGltdW0gZ2FzKSBmb3IgICAgXFxcXFxcbiAgICBcXFxcdGhlIHRyYW5zYWN0aW9uLCBhbmQgdGhlIHByaWNlIGlzIHRoZSBzcG90IHByaWNlIG9mIGdhcyBhdCB0aGF0IHRpbWUuICAgIFxcXFxcXG4gICAgXFxcXFRoZSBnYXMgYnV5IHdpbGwgYmUgZXhlY3V0ZWQgcHJpb3IgdG8gZXhlY3V0aW5nIFNFTkRFUidzIGNvZGUuXFxcIlxcblxcbiAgICBAbW9kZWwgWyhwcm9wZXJ0eSAoPiB0b3RhbCAwLjApKV1cXG5cXG4gICAgKGVuZm9yY2UgKD4gdG90YWwgMC4wKVxcbiAgICAgIFxcXCJnYXMgc3VwcGx5IG11c3QgYmUgYSBwb3NpdGl2ZSBxdWFudGl0eVxcXCIpXFxuXFxuICAgIChyZXF1aXJlLWNhcGFiaWxpdHkgKEZVTkRfVFgpKVxcbiAgICAod2l0aC1jYXBhYmlsaXR5IChUUkFOU0ZFUilcXG4gICAgICAoZGViaXQgc2VuZGVyIHRvdGFsKSlcXG4gICAgKVxcblxcbiAgKGRlZnVuIHJlZGVlbS1nYXM6c3RyaW5nIChtaW5lcjpzdHJpbmcgbWluZXItZ3VhcmQ6Z3VhcmQgc2VuZGVyOnN0cmluZyB0b3RhbDpkZWNpbWFsKVxcbiAgICBAZG9jIFxcXCJUaGlzIGZ1bmN0aW9uIGRlc2NyaWJlcyB0aGUgbWFpbiAncmVkZWVtIGdhcycgb3BlcmF0aW9uLiBBdCB0aGlzICAgIFxcXFxcXG4gICAgXFxcXHBvaW50LCB0aGUgU0VOREVSJ3MgdHJhbnNhY3Rpb24gaGFzIGJlZW4gZXhlY3V0ZWQsIGFuZCB0aGUgZ2FzIHRoYXQgICAgICBcXFxcXFxuICAgIFxcXFx3YXMgY2hhcmdlZCBoYXMgYmVlbiBjYWxjdWxhdGVkLiBNSU5FUiB3aWxsIGJlIGNyZWRpdGVkIHRoZSBnYXMgY29zdCwgICAgXFxcXFxcbiAgICBcXFxcYW5kIFNFTkRFUiB3aWxsIHJlY2VpdmUgdGhlIHJlbWFpbmRlciB1cCB0byB0aGUgbGltaXRcXFwiXFxuXFxuICAgIEBtb2RlbCBbKHByb3BlcnR5ICg-IHRvdGFsIDAuMCkpXVxcblxcbiAgICAocmVxdWlyZS1jYXBhYmlsaXR5IChGVU5EX1RYKSlcXG4gICAgKHdpdGgtY2FwYWJpbGl0eSAoVFJBTlNGRVIpXFxuICAgICAgKGxldCogKChmZWUgKHJlYWQtZGVjaW1hbCBcXFwiZmVlXFxcIikpXFxuICAgICAgICAgICAgIChyZWZ1bmQgKC0gdG90YWwgZmVlKSkpXFxuXFxuICAgICAgICAoZW5mb3JjZSAoPj0gZmVlIDAuMClcXG4gICAgICAgICAgXFxcImZlZSBtdXN0IGJlIGEgbm9uLW5lZ2F0aXZlIHF1YW50aXR5XFxcIilcXG5cXG4gICAgICAgIChlbmZvcmNlICg-PSByZWZ1bmQgMC4wKVxcbiAgICAgICAgICBcXFwicmVmdW4gbXVzdCBiZSBhIG5vbi1uZWdhdGl2ZSBxdWFudGl0eVxcXCIpXFxuXFxuICAgICAgICA7IGRpcmVjdGx5IHVwZGF0ZSBpbnN0ZWFkIG9mIGNyZWRpdFxcbiAgICAgICAgKGlmICg-IHJlZnVuZCAwLjApXFxuICAgICAgICAgICh3aXRoLXJlYWQgY29pbi10YWJsZSBzZW5kZXJcXG4gICAgICAgICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6PSBiYWxhbmNlIH1cXG4gICAgICAgICAgICAodXBkYXRlIGNvaW4tdGFibGUgc2VuZGVyXFxuICAgICAgICAgICAgICB7IFxcXCJiYWxhbmNlXFxcIjogKCsgYmFsYW5jZSByZWZ1bmQpIH0pXFxuICAgICAgICAgICAgKVxcbiAgICAgICAgICBcXFwibm9vcFxcXCIpXFxuXFxuICAgICAgICAoaWYgKD4gZmVlIDAuMClcXG4gICAgICAgICAgKGNyZWRpdCBtaW5lciBtaW5lci1ndWFyZCBmZWUpXFxuICAgICAgICAgIFxcXCJub29wXFxcIilcXG4gICAgICAgICkpXFxuICAgIClcXG5cXG4gIChkZWZ1biBjcmVhdGUtYWNjb3VudDpzdHJpbmcgKGFjY291bnQ6c3RyaW5nIGd1YXJkOmd1YXJkKVxcbiAgICBAZG9jIFxcXCJDcmVhdGUgYW4gYWNjb3VudCBmb3IgQUNDT1VOVCwgd2l0aCBHVUFSRCBjb250cm9sbGluZyBhY2Nlc3MgdG8gdGhlICBcXFxcXFxuICAgIFxcXFxhY2NvdW50LlxcXCJcXG4gICAgKGluc2VydCBjb2luLXRhYmxlIGFjY291bnRcXG4gICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6IDAuMFxcbiAgICAgICwgXFxcImd1YXJkXFxcIiAgIDogZ3VhcmRcXG4gICAgICB9KVxcbiAgICApXFxuXFxuICAoZGVmdW4gYWNjb3VudC1iYWxhbmNlOmRlY2ltYWwgKGFjY291bnQ6c3RyaW5nKVxcbiAgICBAZG9jIFxcXCJDaGVjayBhbiBhY2NvdW50J3MgYmFsYW5jZS5cXFwiXFxuICAgICh3aXRoLXJlYWQgY29pbi10YWJsZSBhY2NvdW50XFxuICAgICAgeyBcXFwiYmFsYW5jZVxcXCIgOj0gYmFsYW5jZSB9XFxuICAgICAgYmFsYW5jZVxcbiAgICAgIClcXG4gICAgKVxcblxcbiAgKGRlZnVuIGFjY291bnQtaW5mbzpvYmplY3QgKGFjY291bnQ6c3RyaW5nKVxcbiAgICBAZG9jIFxcXCJHZXQgYWxsIG9mIGFuIGFjY291bnQncyBpbmZvLiAgVGhpcyBpbmNsdWRlcyB0aGUgYmFsYW5jZSBhbmQgdGhlICAgIFxcXFxcXG4gICAgXFxcXGd1YXJkLlxcXCJcXG4gICAgKHJlYWQgY29pbi10YWJsZSBhY2NvdW50KVxcbiAgICApXFxuXFxuICAoZGVmdW4gcm90YXRlLWFjY291bnQtZ3VhcmQ6c3RyaW5nIChhY2NvdW50OnN0cmluZyBuZXctZ3VhcmQ6Z3VhcmQpXFxuICAgIEBkb2MgXFxcIlJvdGF0ZSBndWFyZCBhc3NvY2lhdGVkIHdpdGggQUNDT1VOVFxcXCJcXG5cXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5IChub3QgKD0gYWNjb3VudCBcXFwiXFxcIikpKSBdXFxuXFxuICAgIChlbmZvcmNlIChub3QgKD0gYWNjb3VudCBcXFwiXFxcIikpXFxuICAgICAgXFxcImFjY291bnQgbmFtZSBtdXN0IGJlIG5vbi1lbXB0eVxcXCIpXFxuXFxuICAgICh3aXRoLXJlYWQgY29pbi10YWJsZSBhY2NvdW50XFxuICAgICAgeyBcXFwiZ3VhcmRcXFwiIDo9IG9sZC1ndWFyZCB9XFxuXFxuICAgICAgKGVuZm9yY2UtZ3VhcmQgb2xkLWd1YXJkKVxcblxcbiAgICAgICh1cGRhdGUgY29pbi10YWJsZSBhY2NvdW50XFxuICAgICAgICB7IFxcXCJndWFyZFxcXCIgOiBuZXctZ3VhcmQgfVxcbiAgICAgICAgKSkpXFxuXFxuXFxuICAoZGVmdW4gdHJhbnNmZXI6c3RyaW5nIChzZW5kZXI6c3RyaW5nIHJlY2VpdmVyOnN0cmluZyBhbW91bnQ6ZGVjaW1hbClcXG4gICAgQGRvYyBcXFwiVHJhbnNmZXIgQU1PVU5UIGJldHdlZW4gYWNjb3VudHMgU0VOREVSIGFuZCBSRUNFSVZFUiBvbiB0aGUgc2FtZSAgICBcXFxcXFxuICAgIFxcXFxjaGFpbi4gVGhpcyBmYWlscyBpZiBlaXRoZXIgU0VOREVSIG9yIFJFQ0VJVkVSIGRvZXMgbm90IGV4aXN0LiAgICAgICAgICAgXFxcXFxcbiAgICBcXFxcQ3JlYXRlLW9uLXRyYW5zZmVyIGNhbiBiZSBkb25lIHVzaW5nIHRoZSAndHJhbnNmZXItYW5kLWNyZWF0ZScgZnVuY3Rpb24uXFxcIlxcblxcbiAgICAoZW5mb3JjZSAobm90ICg9IHNlbmRlciByZWNlaXZlcikpXFxuICAgICAgXFxcInNlbmRlciBjYW5ub3QgYmUgdGhlIHJlY2VpdmVyIG9mIGEgdHJhbnNmZXJcXFwiKVxcblxcbiAgICAoZW5mb3JjZSAoPiBhbW91bnQgMC4wKVxcbiAgICAgIFxcXCJ0cmFuc2ZlciBhbW91bnQgbXVzdCBiZSBwb3NpdGl2ZVxcXCIpXFxuXFxuICAgICh3aXRoLWNhcGFiaWxpdHkgKFRSQU5TRkVSKVxcbiAgICAgIChkZWJpdCBzZW5kZXIgYW1vdW50KVxcbiAgICAgICh3aXRoLXJlYWQgY29pbi10YWJsZSByZWNlaXZlclxcbiAgICAgICAgeyBcXFwiZ3VhcmRcXFwiIDo9IGcgfVxcblxcbiAgICAgICAgKGNyZWRpdCByZWNlaXZlciBnIGFtb3VudCkpXFxuICAgICAgKVxcbiAgICApXFxuXFxuICAoZGVmdW4gdHJhbnNmZXItYW5kLWNyZWF0ZTpzdHJpbmdcXG4gICAgKCBzZW5kZXI6c3RyaW5nXFxuICAgICAgcmVjZWl2ZXI6c3RyaW5nXFxuICAgICAgcmVjZWl2ZXItZ3VhcmQ6Z3VhcmRcXG4gICAgICBhbW91bnQ6ZGVjaW1hbCApXFxuXFxuICAgIEBkb2MgXFxcIlRyYW5zZmVyIGJldHdlZW4gYWNjb3VudHMgU0VOREVSIGFuZCBSRUNFSVZFUiBvbiB0aGUgc2FtZSBjaGFpbi4gICAgXFxcXFxcbiAgICBcXFxcVGhpcyBmYWlscyBpZiB0aGUgU0VOREVSIGFjY291bnQgZG9lcyBub3QgZXhpc3QuIElmIHRoZSBSRUNFSVZFUiBhY2NvdW50IFxcXFxcXG4gICAgXFxcXGRvZXMgbm90IGV4aXN0LCBpdCBpcyBjcmVhdGVkIGFuZCBhc3NvY2lhdGVkIHdpdGggR1VBUkQuXFxcIlxcblxcbiAgICAoZW5mb3JjZSAobm90ICg9IHNlbmRlciByZWNlaXZlcikpXFxuICAgICAgXFxcInNlbmRlciBjYW5ub3QgYmUgdGhlIHJlY2VpdmVyIG9mIGEgdHJhbnNmZXJcXFwiKVxcblxcbiAgICAoZW5mb3JjZSAoPiBhbW91bnQgMC4wKVxcbiAgICAgIFxcXCJ0cmFuc2ZlciBhbW91bnQgbXVzdCBiZSBwb3NpdGl2ZVxcXCIpXFxuXFxuICAgICh3aXRoLWNhcGFiaWxpdHkgKFRSQU5TRkVSKVxcbiAgICAgIChkZWJpdCBzZW5kZXIgYW1vdW50KVxcbiAgICAgIChjcmVkaXQgcmVjZWl2ZXIgcmVjZWl2ZXItZ3VhcmQgYW1vdW50KSlcXG4gICAgKVxcblxcbiAgKGRlZnVuIGNvaW5iYXNlOnN0cmluZyAoYWRkcmVzczpzdHJpbmcgYWRkcmVzcy1ndWFyZDpndWFyZCBhbW91bnQ6ZGVjaW1hbClcXG4gICAgQGRvYyBcXFwiSW50ZXJuYWwgZnVuY3Rpb24gZm9yIHRoZSBpbml0aWFsIGNyZWF0aW9uIG9mIGNvaW5zLiAgVGhpcyBmdW5jdGlvbiBcXFxcXFxuICAgIFxcXFxjYW5ub3QgYmUgdXNlZCBvdXRzaWRlIG9mIHRoZSBjb2luIGNvbnRyYWN0LlxcXCJcXG4gICAgKHJlcXVpcmUtY2FwYWJpbGl0eSAoQ09JTkJBU0UpKVxcbiAgICAod2l0aC1jYXBhYmlsaXR5IChUUkFOU0ZFUilcXG4gICAgIChjcmVkaXQgYWRkcmVzcyBhZGRyZXNzLWd1YXJkIGFtb3VudCkpXFxuICAgIClcXG5cXG4gIChkZWZwYWN0IGZ1bmQtdHggKHNlbmRlciBtaW5lciBtaW5lci1ndWFyZCB0b3RhbClcXG4gICAgQGRvYyBcXFwiJ2Z1bmQtdHgnIGlzIGEgc3BlY2lhbCBwYWN0IHRvIGZ1bmQgYSB0cmFuc2FjdGlvbiBpbiB0d28gc3RlcHMsICAgICBcXFxcXFxuICAgIFxcXFx3aXRoIHRoZSBhY3R1YWwgdHJhbnNhY3Rpb24gdHJhbnNwaXJpbmcgaW4gdGhlIG1pZGRsZTogICAgICAgICAgICAgICAgICAgXFxcXFxcbiAgICBcXFxcICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFxcXFxcXG4gICAgXFxcXCAgMSkgQSBidXlpbmcgcGhhc2UsIGRlYml0aW5nIHRoZSBzZW5kZXIgZm9yIHRvdGFsIGdhcyBhbmQgZmVlLCB5aWVsZGluZyBcXFxcXFxuICAgIFxcXFwgICAgIFRYX01BWF9DSEFSR0UuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXFxcXFxcbiAgICBcXFxcICAyKSBBIHNldHRsZW1lbnQgcGhhc2UsIHJlc3VtaW5nIFRYX01BWF9DSEFSR0UsIGFuZCBhbGxvY2F0aW5nIHRvIHRoZSAgIFxcXFxcXG4gICAgXFxcXCAgICAgY29pbmJhc2UgYWNjb3VudCBmb3IgdXNlZCBnYXMgYW5kIGZlZSwgYW5kIHNlbmRlciBhY2NvdW50IGZvciBiYWwtICBcXFxcXFxuICAgIFxcXFwgICAgIGFuY2UgKHVudXNlZCBnYXMsIGlmIGFueSkuXFxcIlxcblxcbiAgICAoc3RlcCAoYnV5LWdhcyBzZW5kZXIgdG90YWwpKVxcbiAgICAoc3RlcCAocmVkZWVtLWdhcyBtaW5lciBtaW5lci1ndWFyZCBzZW5kZXIgdG90YWwpKVxcbiAgICApXFxuXFxuICAoZGVmdW4gZGViaXQ6c3RyaW5nIChhY2NvdW50OnN0cmluZyBhbW91bnQ6ZGVjaW1hbClcXG4gICAgQGRvYyBcXFwiRGViaXQgQU1PVU5UIGZyb20gQUNDT1VOVCBiYWxhbmNlIHJlY29yZGluZyBEQVRFIGFuZCBEQVRBXFxcIlxcblxcbiAgICBAbW9kZWwgWyAocHJvcGVydHkgKD4gYW1vdW50IDAuMCkpIF1cXG5cXG4gICAgKGVuZm9yY2UgKD4gYW1vdW50IDAuMClcXG4gICAgICBcXFwiZGViaXQgYW1vdW50IG11c3QgYmUgcG9zaXRpdmVcXFwiKVxcblxcbiAgICAocmVxdWlyZS1jYXBhYmlsaXR5IChUUkFOU0ZFUikpXFxuICAgICh3aXRoLWNhcGFiaWxpdHkgKEFDQ09VTlRfR1VBUkQgYWNjb3VudClcXG4gICAgICAod2l0aC1yZWFkIGNvaW4tdGFibGUgYWNjb3VudFxcbiAgICAgICAgeyBcXFwiYmFsYW5jZVxcXCIgOj0gYmFsYW5jZSB9XFxuXFxuICAgICAgICAoZW5mb3JjZSAoPD0gYW1vdW50IGJhbGFuY2UpIFxcXCJJbnN1ZmZpY2llbnQgZnVuZHNcXFwiKVxcbiAgICAgICAgKHVwZGF0ZSBjb2luLXRhYmxlIGFjY291bnRcXG4gICAgICAgICAgeyBcXFwiYmFsYW5jZVxcXCIgOiAoLSBiYWxhbmNlIGFtb3VudCkgfVxcbiAgICAgICAgICApKSlcXG4gICAgKVxcblxcblxcbiAgKGRlZnVuIGNyZWRpdDpzdHJpbmcgKGFjY291bnQ6c3RyaW5nIGd1YXJkOmd1YXJkIGFtb3VudDpkZWNpbWFsKVxcbiAgICBAZG9jIFxcXCJDcmVkaXQgQU1PVU5UIHRvIEFDQ09VTlQgYmFsYW5jZSByZWNvcmRpbmcgREFURSBhbmQgREFUQVxcXCJcXG5cXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5ICg-IGFtb3VudCAwLjApKVxcbiAgICAgICAgICAgICAocHJvcGVydHkgKG5vdCAoPSBhY2NvdW50IFxcXCJcXFwiKSkpXFxuICAgICAgICAgICBdXFxuXFxuICAgIChlbmZvcmNlICg-IGFtb3VudCAwLjApXFxuICAgICAgXFxcImNyZWRpdCBhbW91bnQgbXVzdCBiZSBwb3NpdGl2ZVxcXCIpXFxuXFxuICAgIChyZXF1aXJlLWNhcGFiaWxpdHkgKFRSQU5TRkVSKSlcXG4gICAgKHdpdGgtZGVmYXVsdC1yZWFkIGNvaW4tdGFibGUgYWNjb3VudFxcbiAgICAgIHsgXFxcImJhbGFuY2VcXFwiIDogMC4wLCBcXFwiZ3VhcmRcXFwiIDogZ3VhcmQgfVxcbiAgICAgIHsgXFxcImJhbGFuY2VcXFwiIDo9IGJhbGFuY2UsIFxcXCJndWFyZFxcXCIgOj0gcmV0ZyB9XFxuICAgICAgOyB3ZSBkb24ndCB3YW50IHRvIG92ZXJ3cml0ZSBhbiBleGlzdGluZyBndWFyZCB3aXRoIHRoZSB1c2VyLXN1cHBsaWVkIG9uZVxcbiAgICAgIChlbmZvcmNlICg9IHJldGcgZ3VhcmQpXFxuICAgICAgICBcXFwiYWNjb3VudCBndWFyZHMgZG8gbm90IG1hdGNoXFxcIilcXG5cXG4gICAgICAod3JpdGUgY29pbi10YWJsZSBhY2NvdW50XFxuICAgICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6ICgrIGJhbGFuY2UgYW1vdW50KVxcbiAgICAgICAgLCBcXFwiZ3VhcmRcXFwiICAgOiByZXRnXFxuICAgICAgICB9KVxcbiAgICAgICkpXFxuXFxuICAoZGVmcGFjdCBjcm9zcy1jaGFpbi10cmFuc2ZlclxcbiAgICAoIGRlbGV0ZS1hY2NvdW50OnN0cmluZ1xcbiAgICAgIGNyZWF0ZS1jaGFpbi1pZDpzdHJpbmdcXG4gICAgICBjcmVhdGUtYWNjb3VudDpzdHJpbmdcXG4gICAgICBjcmVhdGUtYWNjb3VudC1ndWFyZDpndWFyZFxcbiAgICAgIHF1YW50aXR5OmRlY2ltYWwgKVxcblxcbiAgICBAZG9jIFxcXCJUcmFuc2ZlciBRVUFOVElUWSBjb2lucyBmcm9tIERFTEVURS1BQ0NPVU5UIG9uIGN1cnJlbnQgY2hhaW4gdG8gICAgICAgICAgIFxcXFxcXG4gICAgICAgICBcXFxcQ1JFQVRFLUFDQ09VTlQgb24gQ1JFQVRFLUNIQUlOLUlELiBUYXJnZXQgY2hhaW4gaWQgbXVzdCBub3QgYmUgdGhlICAgICAgICBcXFxcXFxuICAgICAgICAgXFxcXGN1cnJlbnQgY2hhaW4taWQuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXFxcXFxcbiAgICAgICAgIFxcXFwgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFxcXFxcXG4gICAgICAgICBcXFxcU3RlcCAxOiBCdXJuIFFVQU5USVRZLW1hbnkgY29pbnMgZm9yIERFTEVURS1BQ0NPVU5UIG9uIHRoZSBjdXJyZW50IGNoYWluLCBcXFxcXFxuICAgICAgICAgXFxcXGFuZCBwcm9kdWNlIGFuIFNQViByZWNlaXB0IHdoaWNoIG1heSBiZSBtYW51YWxseSByZWRlZW1lZCBmb3IgYW4gU1BWICAgICAgXFxcXFxcbiAgICAgICAgIFxcXFxwcm9vZi4gT25jZSBhIHByb29mIGlzIG9idGFpbmVkLCB0aGUgdXNlciBtYXkgY2FsbCAnY3JlYXRlLWNvaW4nIGFuZCAgICAgIFxcXFxcXG4gICAgICAgICBcXFxcY29uc3VtZSB0aGUgcHJvb2Ygb24gQ1JFQVRFLUNIQUlOLUlELCBjcmVkaXRpbmcgQ1JFQVRFLUFDQ09VTlQgUVVBTlRJVFktICBcXFxcXFxuICAgICAgICAgXFxcXG1hbnkgY29pbnMuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXFxcXFxcbiAgICAgICAgIFxcXFwgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFxcXFxcXG4gICAgICAgICBcXFxcU3RlcCAyOiBDb25zdW1lIGFuIFNQViBwcm9vZiBmb3IgYSBudW1iZXIgb2YgY29pbnMsIGFuZCBjcmVkaXQgdGhlICAgICAgICBcXFxcXFxuICAgICAgICAgXFxcXGFjY291bnQgYXNzb2NpYXRlZCB3aXRoIHRoZSBwcm9vZiB0aGUgcXVhbnRpZnkgb2YgY29pbnMgYnVybmVkIG9uIHRoZSAgICAgXFxcXFxcbiAgICAgICAgIFxcXFxzb3VyY2UgY2hhaW4gYnkgdGhlIGJ1cm4gYWNjb3VudC4gTm90ZTogbXVzdCBiZSBjYWxsZWQgb24gdGhlIGNvcnJlY3QgICAgIFxcXFxcXG4gICAgICAgICBcXFxcY2hhaW4gaWQgYXMgc3BlY2lmaWVkIGluIHRoZSBwcm9vZi5cXFwiXFxuXFxuICAgIEBtb2RlbCBbIChwcm9wZXJ0eSAoPiBxdWFudGl0eSAwLjApKVxcbiAgICAgICAgICAgLCAocHJvcGVydHkgKG5vdCAoPSBjcmVhdGUtY2hhaW4taWQgXFxcIlxcXCIpKSlcXG4gICAgICAgICAgIF1cXG5cXG4gICAgKHN0ZXBcXG4gICAgICAod2l0aC1jYXBhYmlsaXR5IChUUkFOU0ZFUilcXG4gICAgICAgIChlbmZvcmNlIChub3QgKD0gKGF0ICdjaGFpbi1pZCAoY2hhaW4tZGF0YSkpIGNyZWF0ZS1jaGFpbi1pZCkpXFxuICAgICAgICAgIFxcXCJjYW5ub3QgcnVuIGNyb3NzLWNoYWluIHRyYW5zZmVycyB0byB0aGUgc2FtZSBjaGFpblxcXCIpXFxuXFxuICAgICAgICAoZW5mb3JjZSAoPiBxdWFudGl0eSAwLjApXFxuICAgICAgICAgIFxcXCJ0cmFuc2ZlciBxdWFudGl0eSBtdXN0IGJlIHBvc2l0aXZlXFxcIilcXG5cXG4gICAgICAgIChkZWJpdCBkZWxldGUtYWNjb3VudCBxdWFudGl0eSlcXG4gICAgICAgIChsZXRcXG4gICAgICAgICAgKChyZXR2Om9iamVjdHt0cmFuc2Zlci1zY2hlbWF9XFxuICAgICAgICAgICAgeyBcXFwiY3JlYXRlLWFjY291bnRcXFwiOiBjcmVhdGUtYWNjb3VudFxcbiAgICAgICAgICAgICwgXFxcImNyZWF0ZS1hY2NvdW50LWd1YXJkXFxcIjogY3JlYXRlLWFjY291bnQtZ3VhcmRcXG4gICAgICAgICAgICAsIFxcXCJxdWFudGl0eVxcXCI6IHF1YW50aXR5XFxuICAgICAgICAgICAgfSkpXFxuICAgICAgICAgICh5aWVsZCByZXR2IGNyZWF0ZS1jaGFpbi1pZClcXG4gICAgICAgICAgKSkpXFxuXFxuICAgIChzdGVwXFxuICAgICAgKHJlc3VtZVxcbiAgICAgICAgeyBcXFwiY3JlYXRlLWFjY291bnRcXFwiIDo9IGNyZWF0ZS1hY2NvdW50XFxuICAgICAgICAsIFxcXCJjcmVhdGUtYWNjb3VudC1ndWFyZFxcXCIgOj0gY3JlYXRlLWFjY291bnQtZ3VhcmRcXG4gICAgICAgICwgXFxcInF1YW50aXR5XFxcIiA6PSBxdWFudGl0eVxcbiAgICAgICAgfVxcblxcbiAgICAgICAgKHdpdGgtY2FwYWJpbGl0eSAoVFJBTlNGRVIpXFxuICAgICAgICAgIChjcmVkaXQgY3JlYXRlLWFjY291bnQgY3JlYXRlLWFjY291bnQtZ3VhcmQgcXVhbnRpdHkpKVxcbiAgICAgICAgKSlcXG4gICAgKVxcbilcXG5cXG4oY3JlYXRlLXRhYmxlIGNvaW4tdGFibGUpXFxuXCJ9fSxcInNpZ25lcnNcIjpbXSxcIm1ldGFcIjp7XCJjcmVhdGlvblRpbWVcIjowLFwidHRsXCI6MCxcImdhc0xpbWl0XCI6MCxcImNoYWluSWRcIjpcIlwiLFwiZ2FzUHJpY2VcIjowLFwic2VuZGVyXCI6XCJcIn0sXCJub25jZVwiOlwiXFxcImdlbmVzaXMtMDFcXFwiXCJ9In0
  - eyJnYXMiOjAsInJlc3VsdCI6eyJzdGF0dXMiOiJzdWNjZXNzIiwiZGF0YSI6IlRhYmxlQ3JlYXRlZCJ9LCJyZXFLZXkiOiJmSWpkRmZOUXFoX1JnLWxXZmNLX1o4UC1KMU0yeHE1RjlfdkZLcUlaTUJjIiwibG9ncyI6IjRLVjRZZGpJcmF1OGdoSFhpOF9DaXkwT3VfbmNoSUxZMzc0aUZ6emUzVnMiLCJtZXRhRGF0YSI6bnVsbCwiY29udGludWF0aW9uIjpudWxsLCJ0eElkIjoxfQ
- - eyJoYXNoIjoiYld0S1NINjcxT1BTdlRrZk82UkdHNURhUi03SDJ3MVR6cXA2c3lhd21payIsInNpZ3MiOltdLCJjbWQiOiJ7XCJwYXlsb2FkXCI6e1wiZXhlY1wiOntcImRhdGFcIjp7XCJzZW5kZXIwN1wiOltcIjRjMzFkYzllZTdmMjQxNzdmNzhiNmY1MTgwMTJhMjA4MzI2ZTJhZjFmMzdiYjBhMjQwNWI1MDU2ZDBjYWQ2MjhcIl0sXCJzZW5kZXIwMVwiOltcIjZiZTJmNDg1YTdhZjc1ZmVkYjRiN2YxNTNhOTAzZjdlNjAwMGNhNGFhNTAxMTc5YzkxYTI0NTBiNzc3YmQyYTdcIl0sXCJzZW5kZXIwNlwiOltcIjVmZmMxZjdmZWY3YTQ0NzM4NjI1NzYyZjc1YTQyMjk0NTQ5NTFlMDNmMmFmYzZmODEzMDljMGMxYmRmOWVlNmZcIl0sXCJzZW5kZXIwMFwiOltcIjM2ODgyMGY4MGMzMjRiYmM3YzJiMDYxMDY4OGE3ZGE0M2UzOWY5MWQxMTg3MzI2NzFjZDljNzUwMGZmNDNjY2FcIl0sXCJjcm9lc3VzXCI6W1wiMjk5M2Y3OTVkMTMzZmE1ZDBmZDg3N2E2NDFjYWJjOGIyOGNkMzYxNDdmNjY2OTg4Y2FjYmFhNDM3OWQxZmY5M1wiXSxcInNlbmRlcjA1XCI6W1wiZjA5ZDhmNjM5NGFlYTQyNWZlNjc4M2Q4OGNkODEzNjNkODAxN2YxNmFmZDM3MTFjNTc1YmUwZjVjZDVjOWJiOVwiXSxcInNlbmRlcjA0XCI6W1wiMmQ3MGFhNGY2OTdjM2EzYjhkZDZkOTc3NDVhYzA3NGVkY2ZkMGViNjVjMzc3NzRjZGUyNTEzNTQ4M2JlYTcxZVwiXSxcIm11bHRpLTAyLTAzLTA0LWFueVwiOntcInByZWRcIjpcImtleXMtYW55XCIsXCJrZXlzXCI6W1wiM2E5ZGQ1MzJkNzNkYWNlMTk1ZGJiNjRkMWRiYTY1NzJmYjc4M2QwZmRkMzI0Njg1ZTMyZmJkYTJmODlmOTlhNlwiLFwiNDNmMmFkYjFkZTE5MjAwMGNiMzc3N2JhY2M3Zjk4M2I2NjE0ZmQ5YzE3MTVjZDQ0Y2Q0ODRiNmQzYTBkMzRjOFwiLFwiMmQ3MGFhNGY2OTdjM2EzYjhkZDZkOTc3NDVhYzA3NGVkY2ZkMGViNjVjMzc3NzRjZGUyNTEzNTQ4M2JlYTcxZVwiXX0sXCJzZW5kZXIwOVwiOltcImM1OWQ5ODQwYjBiNjYwOTA4MzY1NDZiN2ViNGE3MzYwNjI1NzUyN2VjOGMyYjQ4MjMwMGZkMjI5MjY0YjA3ZTZcIl0sXCJzZW5kZXIwM1wiOltcIjQzZjJhZGIxZGUxOTIwMDBjYjM3NzdiYWNjN2Y5ODNiNjYxNGZkOWMxNzE1Y2Q0NGNkNDg0YjZkM2EwZDM0YzhcIl0sXCJtdWx0aS0wMC0wMVwiOltcIjM2ODgyMGY4MGMzMjRiYmM3YzJiMDYxMDY4OGE3ZGE0M2UzOWY5MWQxMTg3MzI2NzFjZDljNzUwMGZmNDNjY2FcIixcIjZiZTJmNDg1YTdhZjc1ZmVkYjRiN2YxNTNhOTAzZjdlNjAwMGNhNGFhNTAxMTc5YzkxYTI0NTBiNzc3YmQyYTdcIl0sXCJzZW5kZXIwOFwiOltcIjYzYjJlYmE0ZWQ3MGQ0NjEyZDNlN2JjOTBkYjJmYmY0Yzc2ZjdiMDc0MzYzZTg2ZDczZjBiYzYxN2Y4ZThiODFcIl0sXCJzZW5kZXIwMlwiOltcIjNhOWRkNTMyZDczZGFjZTE5NWRiYjY0ZDFkYmE2NTcyZmI3ODNkMGZkZDMyNDY4NWUzMmZiZGEyZjg5Zjk5YTZcIl19LFwiY29kZVwiOlwiKGNvaW4uY29pbmJhc2UgXFxcImNyb2VzdXNcXFwiIChyZWFkLWtleXNldCBcXFwiY3JvZXN1c1xcXCIpIDkwMDAwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDBcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDBcXFwiKSAxMDAwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDFcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDFcXFwiKSAxMTAwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDJcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDJcXFwiKSAxMjAwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDNcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDNcXFwiKSAxMzAwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDRcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDRcXFwiKSAxNDAwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDVcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDVcXFwiKSAxNTAwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDZcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDZcXFwiKSAxNjAwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDdcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDdcXFwiKSAxNzAwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDhcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDhcXFwiKSAxODAwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDlcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDlcXFwiKSAxOTAwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwibXVsdGktMDAtMDFcXFwiIChyZWFkLWtleXNldCBcXFwibXVsdGktMDAtMDFcXFwiKSAxMDEwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwibXVsdGktMDItMDMtMDQtYW55XFxcIiAocmVhZC1rZXlzZXQgXFxcIm11bHRpLTAyLTAzLTA0LWFueVxcXCIpIDEyMzQwMDAwMC4wKVwifX0sXCJzaWduZXJzXCI6W10sXCJtZXRhXCI6e1wiY3JlYXRpb25UaW1lXCI6MCxcInR0bFwiOjAsXCJnYXNMaW1pdFwiOjAsXCJjaGFpbklkXCI6XCJcIixcImdhc1ByaWNlXCI6MCxcInNlbmRlclwiOlwiXCJ9LFwibm9uY2VcIjpcIlxcXCJ0ZXN0bmV0LWdyYW50c1xcXCJcIn0ifQ
  - eyJnYXMiOjAsInJlc3VsdCI6eyJzdGF0dXMiOiJzdWNjZXNzIiwiZGF0YSI6IkxvYWRlZCBpbnRlcmZhY2UgY29pbi1zaWcifSwicmVxS2V5IjoiN0dFZGdTV3Bid2ZwZk91OUlSYnU5N2RhMEk2Vng2U2VCWENsWGY0TC1MNCIsImxvZ3MiOiJPRHFPT1VNVzZwOTUzVW5kUDlmQzhpS3ZwemszY3d5MEp2elpzVDBnSy1vIiwibWV0YURhdGEiOm51bGwsImNvbnRpbnVhdGlvbiI6bnVsbCwidHhJZCI6MH0
minerData: eyJhY2NvdW50IjoiTm9NaW5lciIsInByZWRpY2F0ZSI6IjwiLCJwdWJsaWMta2V5cyI6W119
transactionsHash: rbfCpw3T73rVn9haJ5QKxKTQC6KfEcss4XXqIS05Hm8
outputsHash: D9Kw5aaCftsBI1ojrg4RY-KSV9C21VE9aHhyOddxF38
payloadHash: jHSPjZGaktW2lj7peH07-UQpkO3Rba4K5zWJ_p0in28
coinbase: eyJnYXMiOjAsInJlc3VsdCI6eyJzdGF0dXMiOiJzdWNjZXNzIiwiZGF0YSI6Ik5PX0NPSU5CQVNFIn0sInJlcUtleSI6IkRsZFJ3Q2JsUTdMb3F5NndZSm5hb2RIbDMwZDNqM2VILXF0RnpmRXY0NmciLCJsb2dzIjpudWxsLCJtZXRhRGF0YSI6bnVsbCwiY29udGludWF0aW9uIjpudWxsLCJ0eElkIjpudWxsfQ

|]
