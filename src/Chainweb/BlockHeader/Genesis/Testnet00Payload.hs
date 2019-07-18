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
- - eyJoYXNoIjoiTld5UGotbTdmbkxMVjVPeXlxRXotREIyMWllZTNqS1JzMjh5Vm45RVpLTSIsInNpZ3MiOltdLCJjbWQiOiJ7XCJwYXlsb2FkXCI6e1wiZXhlY1wiOntcImRhdGFcIjpudWxsLFwiY29kZVwiOlwiXFxuKGludGVyZmFjZSBjb2luLXNpZ1xcblxcbiAgXFxcIidjb2luLXNpZycgcmVwcmVzZW50cyB0aGUgS2FkZW5hIENvaW4gQ29udHJhY3QgaW50ZXJmYWNlLiBUaGlzIGNvbnRyYWN0ICAgICBcXFxcXFxuICBcXFxccHJvdmlkZXMgYm90aCB0aGUgdGhlIGdlbmVyYWwgaW50ZXJmYWNlIGZvciBhIEthZGVuYSdzIHRva2VuLCBzdXBwbHlpbmcgYSAgIFxcXFxcXG4gIFxcXFx0cmFuc2ZlciBmdW5jdGlvbiwgY29pbmJhc2UsIGFjY291bnQgY3JlYXRpb24gYW5kIGJhbGFuY2UgcXVlcnkuXFxcIlxcblxcbiAgKGRlZnVuIGNyZWF0ZS1hY2NvdW50OnN0cmluZyAoYWNjb3VudDpzdHJpbmcgZ3VhcmQ6Z3VhcmQpXFxuICAgIEBkb2MgXFxcIkNyZWF0ZSBhbiBhY2NvdW50IGZvciBBQ0NPVU5ULCB3aXRoIEFDQ09VTlQgYXMgYSBmdW5jdGlvbiBvZiBHVUFSRFxcXCJcXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5IChub3QgKD0gYWNjb3VudCBcXFwiXFxcIikpKSBdXFxuICAgIClcXG5cXG4gIChkZWZ1biB0cmFuc2ZlcjpzdHJpbmcgKHNlbmRlcjpzdHJpbmcgcmVjZWl2ZXI6c3RyaW5nIHJlY2VpdmVyLWd1YXJkOmd1YXJkIGFtb3VudDpkZWNpbWFsKVxcbiAgICBAZG9jIFxcXCJUcmFuc2ZlciBiZXR3ZWVuIGFjY291bnRzIFNFTkRFUiBhbmQgUkVDRUlWRVIgb24gdGhlIHNhbWUgY2hhaW4uICAgIFxcXFxcXG4gICAgXFxcXFRoaXMgZmFpbHMgaWYgYm90aCBhY2NvdW50cyBkbyBub3QgZXhpc3QuIENyZWF0ZS1vbi10cmFuc2ZlciBjYW4gYmUgICAgICBcXFxcXFxuICAgIFxcXFxoYW5kbGVkIGJ5IHNlbmRpbmcgaW4gYSBjcmVhdGUgY29tbWFuZCBpbiB0aGUgc2FtZSB0eC5cXFwiXFxuXFxuICAgIEBtb2RlbCBbIChwcm9wZXJ0eSAoPiBhbW91bnQgMC4wKSlcXG4gICAgICAgICAgICAgKHByb3BlcnR5IChub3QgKD0gc2VuZGVyIHJlY2VpdmVyKSkpXFxuICAgICAgICAgICBdXFxuICAgIClcXG5cXG5cXG4gIChkZWZ1biBhY2NvdW50LWJhbGFuY2U6ZGVjaW1hbCAoYWNjb3VudDpzdHJpbmcpXFxuICAgIEBkb2MgXFxcIlF1ZXJ5IHVzZXIgYWNjb3VudCBBQ0NPVU5UIGJhbGFuY2VcXFwiKVxcblxcbiAgKGRlZnVuIGNvaW5iYXNlOnN0cmluZyAoYWRkcmVzczpzdHJpbmcgYWRkcmVzcy1ndWFyZDpndWFyZCBhbW91bnQ6ZGVjaW1hbClcXG4gICAgQGRvYyBcXFwiTWludCBzb21lIG51bWJlciBvZiB0b2tlbnMgYW5kIGFsbG9jYXRlIHRoZW0gdG8gc29tZSBhZGRyZXNzXFxcIlxcblxcbiAgICBAbW9kZWwgWyAocHJvcGVydHkgKD4gYW1vdW50IDAuMCkpXFxuICAgICAgICAgICAgIChwcm9wZXJ0eSAobm90ICg9IGFkZHJlc3MgXFxcIlxcXCIpKSlcXG4gICAgICAgICAgIF1cXG4gICAgKVxcblxcbilcXG5cIn19LFwic2lnbmVyc1wiOltdLFwibWV0YVwiOntcImdhc0xpbWl0XCI6MCxcImNoYWluSWRcIjpcIlwiLFwiZ2FzUHJpY2VcIjowLFwic2VuZGVyXCI6XCJcIn0sXCJub25jZVwiOlwiXFxcImdlbmVzaXMtMDFcXFwiXCJ9In0
  - eyJnYXMiOjAsInJlc3VsdCI6eyJzdGF0dXMiOiJzdWNjZXNzIiwiZGF0YSI6IldyaXRlIHN1Y2NlZWRlZCJ9LCJyZXFLZXkiOiJQSjJvU2xoQkFVS2dTVWJoUXFjZEU1UFFrWUI0OTNKVXBmc0thTnhyaGNrIiwibG9ncyI6Ik0tY2tlN29DS1QzelIxMGRHUEFKR2doZ1dVTkJmS096emVJLUN6WDd1bEUiLCJtZXRhRGF0YSI6bnVsbCwiY29udGludWF0aW9uIjpudWxsLCJ0eElkIjoyfQ
- - eyJoYXNoIjoiWU9oRTV0QVVoOHNiQ3VZTVJRMDdEcEpRVWsyamZXMV9qMXk1MUJVb1FZZyIsInNpZ3MiOltdLCJjbWQiOiJ7XCJwYXlsb2FkXCI6e1wiZXhlY1wiOntcImRhdGFcIjpudWxsLFwiY29kZVwiOlwiKG1vZHVsZSBjb2luIEdPVkVSTkFOQ0VcXG5cXG4gIFxcXCInY29pbicgcmVwcmVzZW50cyB0aGUgS2FkZW5hIENvaW4gQ29udHJhY3QuIFRoaXMgY29udHJhY3QgcHJvdmlkZXMgYm90aCB0aGUgXFxcXFxcbiAgXFxcXGJ1eS9yZWRlZW0gZ2FzIHN1cHBvcnQgaW4gdGhlIGZvcm0gb2YgJ2Z1bmQtdHgnLCBhcyB3ZWxsIGFzIHRyYW5zZmVyLCAgICAgICBcXFxcXFxuICBcXFxcY3JlZGl0LCBkZWJpdCwgY29pbmJhc2UsIGFjY291bnQgY3JlYXRpb24gYW5kIHF1ZXJ5LCBhcyB3ZWxsIGFzIFNQViBidXJuICAgIFxcXFxcXG4gIFxcXFxjcmVhdGUuIFRvIGFjY2VzcyB0aGUgY29pbiBjb250cmFjdCwgeW91IG1heSB1c2UgaXRzIGZ1bGx5LXF1YWxpZmllZCBuYW1lLCAgXFxcXFxcbiAgXFxcXG9yIGlzc3VlIHRoZSAnKHVzZSBjb2luKScgY29tbWFuZCBpbiB0aGUgYm9keSBvZiBhIG1vZHVsZSBkZWNsYXJhdGlvbi5cXFwiXFxuXFxuXFxuICAoaW1wbGVtZW50cyBjb2luLXNpZylcXG5cXG4gIDsgLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS1cXG4gIDsgU2NoZW1hcyBhbmQgVGFibGVzXFxuXFxuICAoZGVmc2NoZW1hIGNvaW4tc2NoZW1hXFxuICAgIGJhbGFuY2U6ZGVjaW1hbFxcbiAgICBndWFyZDpndWFyZClcXG4gIChkZWZ0YWJsZSBjb2luLXRhYmxlOntjb2luLXNjaGVtYX0pXFxuXFxuICA7IHRoZSBzaGFwZSBvZiBhIGNyb3NzLWNoYWluIHRyYW5zZmVyICh1c2VkIGZvciB0eXBlY2hlY2tpbmcpXFxuICAoZGVmc2NoZW1hIHRyYW5zZmVyLXNjaGVtYVxcbiAgICBjcmVhdGUtYWNjb3VudDpzdHJpbmdcXG4gICAgY3JlYXRlLWFjY291bnQtZ3VhcmQ6Z3VhcmRcXG4gICAgcXVhbnRpdHk6ZGVjaW1hbFxcbiAgICApXFxuXFxuICA7IC0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tXFxuICA7IENhcGFiaWxpdGllc1xcblxcbiAgKGRlZmNhcCBHT1ZFUk5BTkNFICgpXFxuICAgIFxcXCJFbmZvcmNlIG5vbi11cGdyYWRlYWJpbGl0eSBleGNlcHQgaW4gdGhlIGNhc2Ugb2YgYSBoYXJkIGZvcmtcXFwiXFxuICAgIGZhbHNlKVxcblxcbiAgKGRlZmNhcCBUUkFOU0ZFUiAoKVxcbiAgICBcXFwiQXV0b25vbW91cyBjYXBhYmlsaXR5IHRvIHByb3RlY3QgZGViaXQgYW5kIGNyZWRpdCBhY3Rpb25zXFxcIlxcbiAgICB0cnVlKVxcblxcbiAgKGRlZmNhcCBDT0lOQkFTRSAoKVxcbiAgICBcXFwiTWFnaWMgY2FwYWJpbGl0eSB0byBwcm90ZWN0IG1pbmVyIHJld2FyZFxcXCJcXG4gICAgdHJ1ZSlcXG5cXG4gIChkZWZjYXAgRlVORF9UWCAoKVxcbiAgICBcXFwiTWFnaWMgY2FwYWJpbGl0eSB0byBleGVjdXRlIGdhcyBwdXJjaGFzZXMgYW5kIHJlZGVtcHRpb25zXFxcIlxcbiAgICB0cnVlKVxcblxcbiAgKGRlZmNhcCBBQ0NPVU5UX0dVQVJEIChhY2NvdW50KVxcbiAgICBcXFwiTG9va3VwIGFuZCBlbmZvcmNlIGd1YXJkcyBhc3NvY2lhdGVkIHdpdGggYW4gYWNjb3VudFxcXCJcXG4gICAgKHdpdGgtcmVhZCBjb2luLXRhYmxlIGFjY291bnQgeyBcXFwiZ3VhcmRcXFwiIDo9IGcgfVxcbiAgICAgIChlbmZvcmNlLWd1YXJkIGcpKSlcXG5cXG4gIChkZWZjYXAgR09WRVJOQU5DRSAoKVxcbiAgICAoZW5mb3JjZSBmYWxzZSBcXFwiRW5mb3JjZSBub24tdXBncmFkZWFiaWxpdHkgZXhjZXB0IGluIHRoZSBjYXNlIG9mIGEgaGFyZCBmb3JrXFxcIikpXFxuXFxuICA7IC0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tXFxuICA7IENvaW4gQ29udHJhY3RcXG5cXG4gIChkZWZ1biBidXktZ2FzOnN0cmluZyAoc2VuZGVyOnN0cmluZyB0b3RhbDpkZWNpbWFsKVxcbiAgICBAZG9jIFxcXCJUaGlzIGZ1bmN0aW9uIGRlc2NyaWJlcyB0aGUgbWFpbiAnZ2FzIGJ1eScgb3BlcmF0aW9uLiBBdCB0aGlzIHBvaW50IFxcXFxcXG4gICAgXFxcXE1JTkVSIGhhcyBiZWVuIGNob3NlbiBmcm9tIHRoZSBwb29sLCBhbmQgd2lsbCBiZSB2YWxpZGF0ZWQuIFRoZSBTRU5ERVIgICBcXFxcXFxuICAgIFxcXFxvZiB0aGlzIHRyYW5zYWN0aW9uIGhhcyBzcGVjaWZpZWQgYSBnYXMgbGltaXQgTElNSVQgKG1heGltdW0gZ2FzKSBmb3IgICAgXFxcXFxcbiAgICBcXFxcdGhlIHRyYW5zYWN0aW9uLCBhbmQgdGhlIHByaWNlIGlzIHRoZSBzcG90IHByaWNlIG9mIGdhcyBhdCB0aGF0IHRpbWUuICAgIFxcXFxcXG4gICAgXFxcXFRoZSBnYXMgYnV5IHdpbGwgYmUgZXhlY3V0ZWQgcHJpb3IgdG8gZXhlY3V0aW5nIFNFTkRFUidzIGNvZGUuXFxcIlxcblxcbiAgICBAbW9kZWwgWyhwcm9wZXJ0eSAoPiB0b3RhbCAwLjApKV1cXG5cXG4gICAgKGVuZm9yY2UgKD4gdG90YWwgMC4wKVxcbiAgICAgIFxcXCJnYXMgc3VwcGx5IG11c3QgYmUgYSBwb3NpdGl2ZSBxdWFudGl0eVxcXCIpXFxuXFxuICAgIChyZXF1aXJlLWNhcGFiaWxpdHkgKEZVTkRfVFgpKVxcbiAgICAod2l0aC1jYXBhYmlsaXR5IChUUkFOU0ZFUilcXG4gICAgICAoZGViaXQgc2VuZGVyIHRvdGFsKSlcXG4gICAgKVxcblxcbiAgKGRlZnVuIHJlZGVlbS1nYXM6c3RyaW5nIChtaW5lcjpzdHJpbmcgbWluZXItZ3VhcmQ6Z3VhcmQgc2VuZGVyOnN0cmluZyB0b3RhbDpkZWNpbWFsKVxcbiAgICBAZG9jIFxcXCJUaGlzIGZ1bmN0aW9uIGRlc2NyaWJlcyB0aGUgbWFpbiAncmVkZWVtIGdhcycgb3BlcmF0aW9uLiBBdCB0aGlzICAgIFxcXFxcXG4gICAgXFxcXHBvaW50LCB0aGUgU0VOREVSJ3MgdHJhbnNhY3Rpb24gaGFzIGJlZW4gZXhlY3V0ZWQsIGFuZCB0aGUgZ2FzIHRoYXQgICAgICBcXFxcXFxuICAgIFxcXFx3YXMgY2hhcmdlZCBoYXMgYmVlbiBjYWxjdWxhdGVkLiBNSU5FUiB3aWxsIGJlIGNyZWRpdGVkIHRoZSBnYXMgY29zdCwgICAgXFxcXFxcbiAgICBcXFxcYW5kIFNFTkRFUiB3aWxsIHJlY2VpdmUgdGhlIHJlbWFpbmRlciB1cCB0byB0aGUgbGltaXRcXFwiXFxuXFxuICAgIEBtb2RlbCBbKHByb3BlcnR5ICg-IHRvdGFsIDAuMCkpXVxcblxcbiAgICAocmVxdWlyZS1jYXBhYmlsaXR5IChGVU5EX1RYKSlcXG4gICAgKHdpdGgtY2FwYWJpbGl0eSAoVFJBTlNGRVIpXFxuICAgICAgKGxldCogKChmZWUgKHJlYWQtZGVjaW1hbCBcXFwiZmVlXFxcIikpXFxuICAgICAgICAgICAgIChyZWZ1bmQgKC0gdG90YWwgZmVlKSkpXFxuXFxuICAgICAgICAoZW5mb3JjZSAoPj0gZmVlIDAuMClcXG4gICAgICAgICAgXFxcImZlZSBtdXN0IGJlIGEgbm9uLW5lZ2F0aXZlIHF1YW50aXR5XFxcIilcXG5cXG4gICAgICAgIChlbmZvcmNlICg-PSByZWZ1bmQgMC4wKVxcbiAgICAgICAgICBcXFwicmVmdW4gbXVzdCBiZSBhIG5vbi1uZWdhdGl2ZSBxdWFudGl0eVxcXCIpXFxuXFxuICAgICAgICA7IGRpcmVjdGx5IHVwZGF0ZSBpbnN0ZWFkIG9mIGNyZWRpdFxcbiAgICAgICAgKGlmICg-IHJlZnVuZCAwLjApXFxuICAgICAgICAgICh3aXRoLXJlYWQgY29pbi10YWJsZSBzZW5kZXJcXG4gICAgICAgICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6PSBiYWxhbmNlIH1cXG4gICAgICAgICAgICAodXBkYXRlIGNvaW4tdGFibGUgc2VuZGVyXFxuICAgICAgICAgICAgICB7IFxcXCJiYWxhbmNlXFxcIjogKCsgYmFsYW5jZSByZWZ1bmQpIH0pXFxuICAgICAgICAgICAgKVxcbiAgICAgICAgICBcXFwibm9vcFxcXCIpXFxuXFxuICAgICAgICAoaWYgKD4gZmVlIDAuMClcXG4gICAgICAgICAgKGNyZWRpdCBtaW5lciBtaW5lci1ndWFyZCBmZWUpXFxuICAgICAgICAgIFxcXCJub29wXFxcIilcXG4gICAgICAgICkpXFxuICAgIClcXG5cXG4gIChkZWZ1biBjcmVhdGUtYWNjb3VudDpzdHJpbmcgKGFjY291bnQ6c3RyaW5nIGd1YXJkOmd1YXJkKVxcbiAgICAoaW5zZXJ0IGNvaW4tdGFibGUgYWNjb3VudFxcbiAgICAgIHsgXFxcImJhbGFuY2VcXFwiIDogMC4wXFxuICAgICAgLCBcXFwiZ3VhcmRcXFwiICAgOiBndWFyZFxcbiAgICAgIH0pXFxuICAgIClcXG5cXG4gIChkZWZ1biBhY2NvdW50LWJhbGFuY2U6ZGVjaW1hbCAoYWNjb3VudDpzdHJpbmcpXFxuICAgICh3aXRoLXJlYWQgY29pbi10YWJsZSBhY2NvdW50XFxuICAgICAgeyBcXFwiYmFsYW5jZVxcXCIgOj0gYmFsYW5jZSB9XFxuICAgICAgYmFsYW5jZVxcbiAgICAgIClcXG4gICAgKVxcblxcbiAgKGRlZnVuIHRyYW5zZmVyOnN0cmluZyAoc2VuZGVyOnN0cmluZyByZWNlaXZlcjpzdHJpbmcgcmVjZWl2ZXItZ3VhcmQ6Z3VhcmQgYW1vdW50OmRlY2ltYWwpXFxuXFxuICAgIChlbmZvcmNlIChub3QgKD0gc2VuZGVyIHJlY2VpdmVyKSlcXG4gICAgICBcXFwic2VuZGVyIGNhbm5vdCBiZSB0aGUgcmVjZWl2ZXIgb2YgYSB0cmFuc2ZlclxcXCIpXFxuXFxuICAgIChlbmZvcmNlICg-IGFtb3VudCAwLjApXFxuICAgICAgXFxcInRyYW5zZmVyIGFtb3VudCBtdXN0IGJlIHBvc2l0aXZlXFxcIilcXG5cXG4gICAgKHdpdGgtY2FwYWJpbGl0eSAoVFJBTlNGRVIpXFxuICAgICAgKGRlYml0IHNlbmRlciBhbW91bnQpXFxuICAgICAgKGNyZWRpdCByZWNlaXZlciByZWNlaXZlci1ndWFyZCBhbW91bnQpKVxcbiAgICApXFxuXFxuICAoZGVmdW4gY29pbmJhc2U6c3RyaW5nIChhZGRyZXNzOnN0cmluZyBhZGRyZXNzLWd1YXJkOmd1YXJkIGFtb3VudDpkZWNpbWFsKVxcbiAgICAocmVxdWlyZS1jYXBhYmlsaXR5IChDT0lOQkFTRSkpXFxuICAgICh3aXRoLWNhcGFiaWxpdHkgKFRSQU5TRkVSKVxcbiAgICAgKGNyZWRpdCBhZGRyZXNzIGFkZHJlc3MtZ3VhcmQgYW1vdW50KSlcXG4gICAgKVxcblxcbiAgKGRlZnBhY3QgZnVuZC10eCAoc2VuZGVyIG1pbmVyIG1pbmVyLWd1YXJkIHRvdGFsKVxcbiAgICBAZG9jIFxcXCInZnVuZC10eCcgaXMgYSBzcGVjaWFsIHBhY3QgdG8gZnVuZCBhIHRyYW5zYWN0aW9uIGluIHR3byBzdGVwcywgICAgIFxcXFxcXG4gICAgXFxcXHdpdGggdGhlIGFjdHVhbCB0cmFuc2FjdGlvbiB0cmFuc3BpcmluZyBpbiB0aGUgbWlkZGxlOiAgICAgICAgICAgICAgICAgICBcXFxcXFxuICAgIFxcXFwgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXFxcXFxcbiAgICBcXFxcICAxKSBBIGJ1eWluZyBwaGFzZSwgZGViaXRpbmcgdGhlIHNlbmRlciBmb3IgdG90YWwgZ2FzIGFuZCBmZWUsIHlpZWxkaW5nIFxcXFxcXG4gICAgXFxcXCAgICAgVFhfTUFYX0NIQVJHRS4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBcXFxcXFxuICAgIFxcXFwgIDIpIEEgc2V0dGxlbWVudCBwaGFzZSwgcmVzdW1pbmcgVFhfTUFYX0NIQVJHRSwgYW5kIGFsbG9jYXRpbmcgdG8gdGhlICAgXFxcXFxcbiAgICBcXFxcICAgICBjb2luYmFzZSBhY2NvdW50IGZvciB1c2VkIGdhcyBhbmQgZmVlLCBhbmQgc2VuZGVyIGFjY291bnQgZm9yIGJhbC0gIFxcXFxcXG4gICAgXFxcXCAgICAgYW5jZSAodW51c2VkIGdhcywgaWYgYW55KS5cXFwiXFxuXFxuICAgIChzdGVwIChidXktZ2FzIHNlbmRlciB0b3RhbCkpXFxuICAgIChzdGVwIChyZWRlZW0tZ2FzIG1pbmVyIG1pbmVyLWd1YXJkIHNlbmRlciB0b3RhbCkpXFxuICAgIClcXG5cXG4gIChkZWZ1biBkZWJpdDpzdHJpbmcgKGFjY291bnQ6c3RyaW5nIGFtb3VudDpkZWNpbWFsKVxcbiAgICBAZG9jIFxcXCJEZWJpdCBBTU9VTlQgZnJvbSBBQ0NPVU5UIGJhbGFuY2UgcmVjb3JkaW5nIERBVEUgYW5kIERBVEFcXFwiXFxuXFxuICAgIEBtb2RlbCBbIChwcm9wZXJ0eSAoPiBhbW91bnQgMC4wKSkgXVxcblxcbiAgICAoZW5mb3JjZSAoPiBhbW91bnQgMC4wKVxcbiAgICAgIFxcXCJkZWJpdCBhbW91bnQgbXVzdCBiZSBwb3NpdGl2ZVxcXCIpXFxuXFxuICAgIChyZXF1aXJlLWNhcGFiaWxpdHkgKFRSQU5TRkVSKSlcXG4gICAgKHdpdGgtY2FwYWJpbGl0eSAoQUNDT1VOVF9HVUFSRCBhY2NvdW50KVxcbiAgICAgICh3aXRoLXJlYWQgY29pbi10YWJsZSBhY2NvdW50XFxuICAgICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6PSBiYWxhbmNlIH1cXG5cXG4gICAgICAgIChlbmZvcmNlICg8PSBhbW91bnQgYmFsYW5jZSkgXFxcIkluc3VmZmljaWVudCBmdW5kc1xcXCIpXFxuICAgICAgICAodXBkYXRlIGNvaW4tdGFibGUgYWNjb3VudFxcbiAgICAgICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6ICgtIGJhbGFuY2UgYW1vdW50KSB9XFxuICAgICAgICAgICkpKVxcbiAgICApXFxuXFxuXFxuICAoZGVmdW4gY3JlZGl0OnN0cmluZyAoYWNjb3VudDpzdHJpbmcgZ3VhcmQ6Z3VhcmQgYW1vdW50OmRlY2ltYWwpXFxuICAgIEBkb2MgXFxcIkNyZWRpdCBBTU9VTlQgdG8gQUNDT1VOVCBiYWxhbmNlIHJlY29yZGluZyBEQVRFIGFuZCBEQVRBXFxcIlxcblxcbiAgICBAbW9kZWwgWyAocHJvcGVydHkgKD4gYW1vdW50IDAuMCkpXFxuICAgICAgICAgICAgIChwcm9wZXJ0eSAobm90ICg9IGFjY291bnQgXFxcIlxcXCIpKSlcXG4gICAgICAgICAgIF1cXG5cXG4gICAgKGVuZm9yY2UgKD4gYW1vdW50IDAuMClcXG4gICAgICBcXFwiY3JlZGl0IGFtb3VudCBtdXN0IGJlIHBvc2l0aXZlXFxcIilcXG5cXG4gICAgKHJlcXVpcmUtY2FwYWJpbGl0eSAoVFJBTlNGRVIpKVxcbiAgICAod2l0aC1kZWZhdWx0LXJlYWQgY29pbi10YWJsZSBhY2NvdW50XFxuICAgICAgeyBcXFwiYmFsYW5jZVxcXCIgOiAwLjAsIFxcXCJndWFyZFxcXCIgOiBndWFyZCB9XFxuICAgICAgeyBcXFwiYmFsYW5jZVxcXCIgOj0gYmFsYW5jZSwgXFxcImd1YXJkXFxcIiA6PSByZXRnIH1cXG4gICAgICA7IHdlIGRvbid0IHdhbnQgdG8gb3ZlcndyaXRlIGFuIGV4aXN0aW5nIGd1YXJkIHdpdGggdGhlIHVzZXItc3VwcGxpZWQgb25lXFxuICAgICAgKGVuZm9yY2UgKD0gcmV0ZyBndWFyZClcXG4gICAgICAgIFxcXCJhY2NvdW50IGd1YXJkcyBkbyBub3QgbWF0Y2hcXFwiKVxcblxcbiAgICAgICh3cml0ZSBjb2luLXRhYmxlIGFjY291bnRcXG4gICAgICAgIHsgXFxcImJhbGFuY2VcXFwiIDogKCsgYmFsYW5jZSBhbW91bnQpXFxuICAgICAgICAsIFxcXCJndWFyZFxcXCIgICA6IHJldGdcXG4gICAgICAgIH0pXFxuICAgICAgKSlcXG5cXG4gIChkZWZwYWN0IGNyb3NzLWNoYWluLXRyYW5zZmVyXFxuICAgICggZGVsZXRlLWFjY291bnQ6c3RyaW5nXFxuICAgICAgY3JlYXRlLWNoYWluLWlkOnN0cmluZ1xcbiAgICAgIGNyZWF0ZS1hY2NvdW50OnN0cmluZ1xcbiAgICAgIGNyZWF0ZS1hY2NvdW50LWd1YXJkOmd1YXJkXFxuICAgICAgcXVhbnRpdHk6ZGVjaW1hbCApXFxuXFxuICAgIEBkb2MgXFxcIlRyYW5zZmVyIFFVQU5USVRZIGNvaW5zIGZyb20gREVMRVRFLUFDQ09VTlQgb24gY3VycmVudCBjaGFpbiB0byAgICAgICAgICAgXFxcXFxcbiAgICAgICAgIFxcXFxDUkVBVEUtQUNDT1VOVCBvbiBDUkVBVEUtQ0hBSU4tSUQuIFRhcmdldCBjaGFpbiBpZCBtdXN0IG5vdCBiZSB0aGUgICAgICAgIFxcXFxcXG4gICAgICAgICBcXFxcY3VycmVudCBjaGFpbi1pZC4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBcXFxcXFxuICAgICAgICAgXFxcXCAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXFxcXFxcbiAgICAgICAgIFxcXFxTdGVwIDE6IEJ1cm4gUVVBTlRJVFktbWFueSBjb2lucyBmb3IgREVMRVRFLUFDQ09VTlQgb24gdGhlIGN1cnJlbnQgY2hhaW4sIFxcXFxcXG4gICAgICAgICBcXFxcYW5kIHByb2R1Y2UgYW4gU1BWIHJlY2VpcHQgd2hpY2ggbWF5IGJlIG1hbnVhbGx5IHJlZGVlbWVkIGZvciBhbiBTUFYgICAgICBcXFxcXFxuICAgICAgICAgXFxcXHByb29mLiBPbmNlIGEgcHJvb2YgaXMgb2J0YWluZWQsIHRoZSB1c2VyIG1heSBjYWxsICdjcmVhdGUtY29pbicgYW5kICAgICAgXFxcXFxcbiAgICAgICAgIFxcXFxjb25zdW1lIHRoZSBwcm9vZiBvbiBDUkVBVEUtQ0hBSU4tSUQsIGNyZWRpdGluZyBDUkVBVEUtQUNDT1VOVCBRVUFOVElUWS0gIFxcXFxcXG4gICAgICAgICBcXFxcbWFueSBjb2lucy4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBcXFxcXFxuICAgICAgICAgXFxcXCAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXFxcXFxcbiAgICAgICAgIFxcXFxTdGVwIDI6IENvbnN1bWUgYW4gU1BWIHByb29mIGZvciBhIG51bWJlciBvZiBjb2lucywgYW5kIGNyZWRpdCB0aGUgICAgICAgIFxcXFxcXG4gICAgICAgICBcXFxcYWNjb3VudCBhc3NvY2lhdGVkIHdpdGggdGhlIHByb29mIHRoZSBxdWFudGlmeSBvZiBjb2lucyBidXJuZWQgb24gdGhlICAgICBcXFxcXFxuICAgICAgICAgXFxcXHNvdXJjZSBjaGFpbiBieSB0aGUgYnVybiBhY2NvdW50LiBOb3RlOiBtdXN0IGJlIGNhbGxlZCBvbiB0aGUgY29ycmVjdCAgICAgXFxcXFxcbiAgICAgICAgIFxcXFxjaGFpbiBpZCBhcyBzcGVjaWZpZWQgaW4gdGhlIHByb29mLlxcXCJcXG5cXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5ICg-IHF1YW50aXR5IDAuMCkpXFxuICAgICAgICAgICAsIChwcm9wZXJ0eSAobm90ICg9IGNyZWF0ZS1jaGFpbi1pZCBcXFwiXFxcIikpKVxcbiAgICAgICAgICAgXVxcblxcbiAgICAoc3RlcFxcbiAgICAgICh3aXRoLWNhcGFiaWxpdHkgKFRSQU5TRkVSKVxcbiAgICAgICAgKGVuZm9yY2UgKG5vdCAoPSAoYXQgJ2NoYWluLWlkIChjaGFpbi1kYXRhKSkgY3JlYXRlLWNoYWluLWlkKSlcXG4gICAgICAgICAgXFxcImNhbm5vdCBydW4gY3Jvc3MtY2hhaW4gdHJhbnNmZXJzIHRvIHRoZSBzYW1lIGNoYWluXFxcIilcXG5cXG4gICAgICAgIChlbmZvcmNlICg-IHF1YW50aXR5IDAuMClcXG4gICAgICAgICAgXFxcInRyYW5zZmVyIHF1YW50aXR5IG11c3QgYmUgcG9zaXRpdmVcXFwiKVxcblxcbiAgICAgICAgKGRlYml0IGRlbGV0ZS1hY2NvdW50IHF1YW50aXR5KVxcbiAgICAgICAgKGxldFxcbiAgICAgICAgICAoKHJldHY6b2JqZWN0e3RyYW5zZmVyLXNjaGVtYX1cXG4gICAgICAgICAgICB7IFxcXCJjcmVhdGUtYWNjb3VudFxcXCI6IGNyZWF0ZS1hY2NvdW50XFxuICAgICAgICAgICAgLCBcXFwiY3JlYXRlLWFjY291bnQtZ3VhcmRcXFwiOiBjcmVhdGUtYWNjb3VudC1ndWFyZFxcbiAgICAgICAgICAgICwgXFxcInF1YW50aXR5XFxcIjogcXVhbnRpdHlcXG4gICAgICAgICAgICB9KSlcXG4gICAgICAgICAgKHlpZWxkIHJldHYgY3JlYXRlLWNoYWluLWlkKVxcbiAgICAgICAgICApKSlcXG5cXG4gICAgKHN0ZXBcXG4gICAgICAocmVzdW1lXFxuICAgICAgICB7IFxcXCJjcmVhdGUtYWNjb3VudFxcXCIgOj0gY3JlYXRlLWFjY291bnRcXG4gICAgICAgICwgXFxcImNyZWF0ZS1hY2NvdW50LWd1YXJkXFxcIiA6PSBjcmVhdGUtYWNjb3VudC1ndWFyZFxcbiAgICAgICAgLCBcXFwicXVhbnRpdHlcXFwiIDo9IHF1YW50aXR5XFxuICAgICAgICB9XFxuXFxuICAgICAgICAod2l0aC1jYXBhYmlsaXR5IChUUkFOU0ZFUilcXG4gICAgICAgICAgKGNyZWRpdCBjcmVhdGUtYWNjb3VudCBjcmVhdGUtYWNjb3VudC1ndWFyZCBxdWFudGl0eSkpXFxuICAgICAgICApKVxcbiAgICApXFxuKVxcblxcbihjcmVhdGUtdGFibGUgY29pbi10YWJsZSlcXG5cIn19LFwic2lnbmVyc1wiOltdLFwibWV0YVwiOntcImdhc0xpbWl0XCI6MCxcImNoYWluSWRcIjpcIlwiLFwiZ2FzUHJpY2VcIjowLFwic2VuZGVyXCI6XCJcIn0sXCJub25jZVwiOlwiXFxcImdlbmVzaXMtMDFcXFwiXCJ9In0
  - eyJnYXMiOjAsInJlc3VsdCI6eyJzdGF0dXMiOiJzdWNjZXNzIiwiZGF0YSI6IlRhYmxlQ3JlYXRlZCJ9LCJyZXFLZXkiOiJZT2hFNXRBVWg4c2JDdVlNUlEwN0RwSlFVazJqZlcxX2oxeTUxQlVvUVlnIiwibG9ncyI6IlhhSXhNUDVEbW9uNTBnUFJXZTJ5ai1DQ21JYVhicUJlYmxGRWZGS0gySTAiLCJtZXRhRGF0YSI6bnVsbCwiY29udGludWF0aW9uIjpudWxsLCJ0eElkIjoxfQ
- - eyJoYXNoIjoiUEoyb1NsaEJBVUtnU1ViaFFxY2RFNVBRa1lCNDkzSlVwZnNLYU54cmhjayIsInNpZ3MiOltdLCJjbWQiOiJ7XCJwYXlsb2FkXCI6e1wiZXhlY1wiOntcImRhdGFcIjp7XCJzZW5kZXIwN1wiOltcIjRjMzFkYzllZTdmMjQxNzdmNzhiNmY1MTgwMTJhMjA4MzI2ZTJhZjFmMzdiYjBhMjQwNWI1MDU2ZDBjYWQ2MjhcIl0sXCJzZW5kZXIwMVwiOltcIjZiZTJmNDg1YTdhZjc1ZmVkYjRiN2YxNTNhOTAzZjdlNjAwMGNhNGFhNTAxMTc5YzkxYTI0NTBiNzc3YmQyYTdcIl0sXCJzZW5kZXIwNlwiOltcIjVmZmMxZjdmZWY3YTQ0NzM4NjI1NzYyZjc1YTQyMjk0NTQ5NTFlMDNmMmFmYzZmODEzMDljMGMxYmRmOWVlNmZcIl0sXCJzZW5kZXIwMFwiOltcIjM2ODgyMGY4MGMzMjRiYmM3YzJiMDYxMDY4OGE3ZGE0M2UzOWY5MWQxMTg3MzI2NzFjZDljNzUwMGZmNDNjY2FcIl0sXCJzZW5kZXIwNVwiOltcImYwOWQ4ZjYzOTRhZWE0MjVmZTY3ODNkODhjZDgxMzYzZDgwMTdmMTZhZmQzNzExYzU3NWJlMGY1Y2Q1YzliYjlcIl0sXCJzZW5kZXIwNFwiOltcIjJkNzBhYTRmNjk3YzNhM2I4ZGQ2ZDk3NzQ1YWMwNzRlZGNmZDBlYjY1YzM3Nzc0Y2RlMjUxMzU0ODNiZWE3MWVcIl0sXCJtdWx0aS0wMi0wMy0wNC1hbnlcIjp7XCJwcmVkXCI6XCJrZXlzLWFueVwiLFwia2V5c1wiOltcIjNhOWRkNTMyZDczZGFjZTE5NWRiYjY0ZDFkYmE2NTcyZmI3ODNkMGZkZDMyNDY4NWUzMmZiZGEyZjg5Zjk5YTZcIixcIjQzZjJhZGIxZGUxOTIwMDBjYjM3NzdiYWNjN2Y5ODNiNjYxNGZkOWMxNzE1Y2Q0NGNkNDg0YjZkM2EwZDM0YzhcIixcIjJkNzBhYTRmNjk3YzNhM2I4ZGQ2ZDk3NzQ1YWMwNzRlZGNmZDBlYjY1YzM3Nzc0Y2RlMjUxMzU0ODNiZWE3MWVcIl19LFwic2VuZGVyMDlcIjpbXCJjNTlkOTg0MGIwYjY2MDkwODM2NTQ2YjdlYjRhNzM2MDYyNTc1MjdlYzhjMmI0ODIzMDBmZDIyOTI2NGIwN2U2XCJdLFwic2VuZGVyMDNcIjpbXCI0M2YyYWRiMWRlMTkyMDAwY2IzNzc3YmFjYzdmOTgzYjY2MTRmZDljMTcxNWNkNDRjZDQ4NGI2ZDNhMGQzNGM4XCJdLFwibXVsdGktMDAtMDFcIjpbXCIzNjg4MjBmODBjMzI0YmJjN2MyYjA2MTA2ODhhN2RhNDNlMzlmOTFkMTE4NzMyNjcxY2Q5Yzc1MDBmZjQzY2NhXCIsXCI2YmUyZjQ4NWE3YWY3NWZlZGI0YjdmMTUzYTkwM2Y3ZTYwMDBjYTRhYTUwMTE3OWM5MWEyNDUwYjc3N2JkMmE3XCJdLFwic2VuZGVyMDhcIjpbXCI2M2IyZWJhNGVkNzBkNDYxMmQzZTdiYzkwZGIyZmJmNGM3NmY3YjA3NDM2M2U4NmQ3M2YwYmM2MTdmOGU4YjgxXCJdLFwic2VuZGVyMDJcIjpbXCIzYTlkZDUzMmQ3M2RhY2UxOTVkYmI2NGQxZGJhNjU3MmZiNzgzZDBmZGQzMjQ2ODVlMzJmYmRhMmY4OWY5OWE2XCJdfSxcImNvZGVcIjpcIihjb2luLmNvaW5iYXNlIFxcXCJzZW5kZXIwMFxcXCIgKHJlYWQta2V5c2V0IFxcXCJzZW5kZXIwMFxcXCIpIDEwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDFcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDFcXFwiKSAxMDEwLjApXFxuKGNvaW4uY29pbmJhc2UgXFxcInNlbmRlcjAyXFxcIiAocmVhZC1rZXlzZXQgXFxcInNlbmRlcjAyXFxcIikgMTAyMC4wKVxcbihjb2luLmNvaW5iYXNlIFxcXCJzZW5kZXIwM1xcXCIgKHJlYWQta2V5c2V0IFxcXCJzZW5kZXIwM1xcXCIpIDEwMzAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDRcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDRcXFwiKSAxMDQwLjApXFxuKGNvaW4uY29pbmJhc2UgXFxcInNlbmRlcjA1XFxcIiAocmVhZC1rZXlzZXQgXFxcInNlbmRlcjA1XFxcIikgMTA1MC4wKVxcbihjb2luLmNvaW5iYXNlIFxcXCJzZW5kZXIwNlxcXCIgKHJlYWQta2V5c2V0IFxcXCJzZW5kZXIwNlxcXCIpIDEwNjAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDdcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDdcXFwiKSAxMDcwLjApXFxuKGNvaW4uY29pbmJhc2UgXFxcInNlbmRlcjA4XFxcIiAocmVhZC1rZXlzZXQgXFxcInNlbmRlcjA4XFxcIikgMTA4MC4wKVxcbihjb2luLmNvaW5iYXNlIFxcXCJzZW5kZXIwOVxcXCIgKHJlYWQta2V5c2V0IFxcXCJzZW5kZXIwOVxcXCIpIDEwOTAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwibXVsdGktMDAtMDFcXFwiIChyZWFkLWtleXNldCBcXFwibXVsdGktMDAtMDFcXFwiKSAxMDAxLjApXFxuKGNvaW4uY29pbmJhc2UgXFxcIm11bHRpLTAyLTAzLTA0LWFueVxcXCIgKHJlYWQta2V5c2V0IFxcXCJtdWx0aS0wMi0wMy0wNC1hbnlcXFwiKSAxMjM0LjApXCJ9fSxcInNpZ25lcnNcIjpbXSxcIm1ldGFcIjp7XCJnYXNMaW1pdFwiOjAsXCJjaGFpbklkXCI6XCJcIixcImdhc1ByaWNlXCI6MCxcInNlbmRlclwiOlwiXCJ9LFwibm9uY2VcIjpcIlxcXCJ0ZXN0bmV0MDAtZ3JhbnRzXFxcIlwifSJ9
  - eyJnYXMiOjAsInJlc3VsdCI6eyJzdGF0dXMiOiJzdWNjZXNzIiwiZGF0YSI6IkxvYWRlZCBpbnRlcmZhY2UgY29pbi1zaWcifSwicmVxS2V5IjoiTld5UGotbTdmbkxMVjVPeXlxRXotREIyMWllZTNqS1JzMjh5Vm45RVpLTSIsImxvZ3MiOiJTQlVub0RRcU4zT2lGNE5LT3o2QUpVeVp2SThGdFlqUlBNZmtGakVKRFVRIiwibWV0YURhdGEiOm51bGwsImNvbnRpbnVhdGlvbiI6bnVsbCwidHhJZCI6MH0
minerData: eyJtIjoiTm9NaW5lciIsImtzIjpbXSwia3AiOiI8In0
transactionsHash: 2sa1Sywjz1BtLlU-YUQWiQ6WY_pxmArR1Q0Y4ECeV7g
outputsHash: 0NoLFK28La040NXHmSlE2YF_ckZ7n2QZ7TNhSdN2jtI
payloadHash: 4li4hrbA4kbWJ6rEmnBniK6XHktb_8nbJ5zm355iHWk
coinbase: eyJnYXMiOjAsInJlc3VsdCI6eyJzdGF0dXMiOiJzdWNjZXNzIiwiZGF0YSI6Ik5PX0NPSU5CQVNFIn0sInJlcUtleSI6IkRsZFJ3Q2JsUTdMb3F5NndZSm5hb2RIbDMwZDNqM2VILXF0RnpmRXY0NmciLCJsb2dzIjpudWxsLCJtZXRhRGF0YSI6bnVsbCwiY29udGludWF0aW9uIjpudWxsLCJ0eElkIjpudWxsfQ

|]
