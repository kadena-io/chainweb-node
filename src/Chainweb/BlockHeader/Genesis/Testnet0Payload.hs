{-# LANGUAGE QuasiQuotes #-}

-- This module is auto-generated. DO NOT EDIT IT MANUALLY.

module Chainweb.BlockHeader.Genesis.Testnet0Payload ( payloadBlock ) where

import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (decodeThrow)

import NeatInterpolation (text)

import Chainweb.Payload (PayloadWithOutputs)
import Chainweb.Utils (fromJuste)

payloadBlock :: PayloadWithOutputs
payloadBlock = fromJuste $ decodeThrow $ encodeUtf8 [text|
transactions:
- - eyJoYXNoIjoiUHF2REVEMVdVaUhGNms4NmY2QUdrWVBjZm5hcENLbmZzRDFiNE03VTFVQSIsInNpZ3MiOltdLCJjbWQiOiJ7XCJuZXR3b3JrSWRcIjpudWxsLFwicGF5bG9hZFwiOntcImV4ZWNcIjp7XCJkYXRhXCI6bnVsbCxcImNvZGVcIjpcIihtb2R1bGUgY29pbiBHT1ZFUk5BTkNFXFxuXFxuICBAZG9jIFxcXCInY29pbicgcmVwcmVzZW50cyB0aGUgS2FkZW5hIENvaW4gQ29udHJhY3QuIFRoaXMgY29udHJhY3QgcHJvdmlkZXMgYm90aCB0aGUgXFxcXFxcbiAgXFxcXGJ1eS9yZWRlZW0gZ2FzIHN1cHBvcnQgaW4gdGhlIGZvcm0gb2YgJ2Z1bmQtdHgnLCBhcyB3ZWxsIGFzIHRyYW5zZmVyLCAgICAgICBcXFxcXFxuICBcXFxcY3JlZGl0LCBkZWJpdCwgY29pbmJhc2UsIGFjY291bnQgY3JlYXRpb24gYW5kIHF1ZXJ5LCBhcyB3ZWxsIGFzIFNQViBidXJuICAgIFxcXFxcXG4gIFxcXFxjcmVhdGUuIFRvIGFjY2VzcyB0aGUgY29pbiBjb250cmFjdCwgeW91IG1heSB1c2UgaXRzIGZ1bGx5LXF1YWxpZmllZCBuYW1lLCAgXFxcXFxcbiAgXFxcXG9yIGlzc3VlIHRoZSAnKHVzZSBjb2luKScgY29tbWFuZCBpbiB0aGUgYm9keSBvZiBhIG1vZHVsZSBkZWNsYXJhdGlvbi5cXFwiXFxuXFxuICBAbW9kZWxcXG4gICAgWyAoZGVmcHJvcGVydHkgY29uc2VydmVzLW1hc3NcXG4gICAgICAgICg9IChjb2x1bW4tZGVsdGEgY29pbi10YWJsZSAnYmFsYW5jZSkgMC4wKSlcXG5cXG4gICAgICAoZGVmcHJvcGVydHkgdmFsaWQtYWNjb3VudCAoYWNjb3VudDpzdHJpbmcpXFxuICAgICAgICAoYW5kXFxuICAgICAgICAgICg-PSAobGVuZ3RoIGFjY291bnQpIDMpXFxuICAgICAgICAgICg8PSAobGVuZ3RoIGFjY291bnQpIDI1NikpKVxcbiAgICBdXFxuXFxuICA7IC0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tXFxuICA7IFNjaGVtYXMgYW5kIFRhYmxlc1xcblxcbiAgKGRlZnNjaGVtYSBjb2luLXNjaGVtYVxcbiAgICBAZG9jIFxcXCJUaGUgY29pbiBjb250cmFjdCB0b2tlbiBzY2hlbWFcXFwiXFxuICAgIDtAbW9kZWwgWyAoaW52YXJpYW50ICg-PSBiYWxhbmNlIDAuMCkpIF0gOyBGViBwcm9ibGVtXFxuXFxuICAgIGJhbGFuY2U6ZGVjaW1hbFxcbiAgICBndWFyZDpndWFyZClcXG5cXG4gIChkZWZ0YWJsZSBjb2luLXRhYmxlOntjb2luLXNjaGVtYX0pXFxuXFxuICAoZGVmc2NoZW1hIHRyYW5zZmVyLXNjaGVtYVxcbiAgICBAZG9jIFxcXCJTY2hlbWEgZm9yIHlpZWxkZWQgdmFsdWUgaW4gY3Jvc3MtY2hhaW4gdHJhbnNmZXJzXFxcIlxcblxcbiAgICBjcmVhdGUtYWNjb3VudDpzdHJpbmdcXG4gICAgY3JlYXRlLWFjY291bnQtZ3VhcmQ6Z3VhcmRcXG4gICAgcXVhbnRpdHk6ZGVjaW1hbClcXG5cXG4gIDsgLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS1cXG4gIDsgQ2FwYWJpbGl0aWVzXFxuXFxuICAoZGVmY2FwIFRSQU5TRkVSICgpXFxuICAgIFxcXCJBdXRvbm9tb3VzIGNhcGFiaWxpdHkgdG8gcHJvdGVjdCBkZWJpdCBhbmQgY3JlZGl0IGFjdGlvbnNcXFwiXFxuICAgIHRydWUpXFxuXFxuICAoZGVmY2FwIENPSU5CQVNFICgpXFxuICAgIFxcXCJNYWdpYyBjYXBhYmlsaXR5IHRvIHByb3RlY3QgbWluZXIgcmV3YXJkXFxcIlxcbiAgICB0cnVlKVxcblxcbiAgKGRlZmNhcCBHRU5FU0lTICgpXFxuICAgIFxcXCJNYWdpYyBjYXBhYmlsaXR5IGNvbnN0cmFpbmluZyBnZW5lc2lzIHRyYW5zYWN0aW9uc1xcXCJcXG4gICAgdHJ1ZSlcXG5cXG4gIChkZWZjYXAgRlVORF9UWCAoKVxcbiAgICBcXFwiTWFnaWMgY2FwYWJpbGl0eSB0byBleGVjdXRlIGdhcyBwdXJjaGFzZXMgYW5kIHJlZGVtcHRpb25zXFxcIlxcbiAgICB0cnVlKVxcblxcbiAgKGRlZmNhcCBBQ0NPVU5UX0dVQVJEIChhY2NvdW50KVxcbiAgICBcXFwiTG9va3VwIGFuZCBlbmZvcmNlIGd1YXJkcyBhc3NvY2lhdGVkIHdpdGggYW4gYWNjb3VudFxcXCJcXG4gICAgKHdpdGgtcmVhZCBjb2luLXRhYmxlIGFjY291bnQgeyBcXFwiZ3VhcmRcXFwiIDo9IGcgfVxcbiAgICAgIChlbmZvcmNlLWd1YXJkIGcpKSlcXG5cXG4gIChkZWZjYXAgR09WRVJOQU5DRSAoKVxcbiAgICAoZW5mb3JjZSBmYWxzZSBcXFwiRW5mb3JjZSBub24tdXBncmFkZWFiaWxpdHkgZXhjZXB0IGluIHRoZSBjYXNlIG9mIGEgaGFyZCBmb3JrXFxcIikpXFxuXFxuXFxuICA7IC0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tXFxuICA7IENvbnN0YW50c1xcblxcbiAgKGRlZmNvbnN0IENPSU5fQ0hBUlNFVCBDSEFSU0VUX0xBVElOMVxcbiAgICBcXFwiVGhlIGRlZmF1bHQgY29pbiBjb250cmFjdCBjaGFyYWN0ZXIgc2V0XFxcIilcXG5cXG4gIChkZWZjb25zdCBNSU5JTVVNX1BSRUNJU0lPTiAxMlxcbiAgICBcXFwiTWluaW11bSBhbGxvd2VkIHByZWNpc2lvbiBmb3IgY29pbiB0cmFuc2FjdGlvbnNcXFwiKVxcblxcbiAgKGRlZmNvbnN0IE1JTklNVU1fQUNDT1VOVF9MRU5HVEggM1xcbiAgICBcXFwiTWluaW11bSBhY2NvdW50IGxlbmd0aCBhZG1pc3NpYmxlIGZvciBjb2luIGFjY291bnRzXFxcIilcXG5cXG4gIChkZWZjb25zdCBNQVhJTVVNX0FDQ09VTlRfTEVOR1RIIDI1NlxcbiAgICBcXFwiTWF4aW11bSBhY2NvdW50IG5hbWUgbGVuZ3RoIGFkbWlzc2libGUgZm9yIGNvaW4gYWNjb3VudHNcXFwiKVxcblxcbiAgOyAtLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLVxcbiAgOyBVdGlsaXRpZXNcXG5cXG4gIChkZWZ1biBlbmZvcmNlLXVuaXQgKGFtb3VudDpkZWNpbWFsKVxcbiAgICBAZG9jIFxcXCJFbmZvcmNlIG1pbmltdW0gcHJlY2lzaW9uIGFsbG93ZWQgZm9yIGNvaW4gdHJhbnNhY3Rpb25zXFxcIlxcblxcbiAgICAoZW5mb3JjZVxcbiAgICAgICg9IChmbG9vciBhbW91bnQgTUlOSU1VTV9QUkVDSVNJT04pXFxuICAgICAgICAgYW1vdW50KVxcbiAgICAgIChmb3JtYXQgXFxcIkFtb3VudCB2aW9sYXRlcyBtaW5pbXVtIHByZWNpc2lvbjoge31cXFwiIFthbW91bnRdKSlcXG4gICAgKVxcblxcbiAgKGRlZnVuIHZhbGlkYXRlLWFjY291bnQgKGFjY291bnQ6c3RyaW5nKVxcbiAgICBAZG9jIFxcXCJFbmZvcmNlIHRoYXQgYW4gYWNjb3VudCBuYW1lIGNvbmZvcm1zIHRvIHRoZSBjb2luIGNvbnRyYWN0IFxcXFxcXG4gICAgICAgICBcXFxcbWluaW11bSBhbmQgbWF4aW11bSBsZW5ndGggcmVxdWlyZW1lbnRzLCBhcyB3ZWxsIGFzIHRoZSAgICBcXFxcXFxuICAgICAgICAgXFxcXGxhdGluLTEgY2hhcmFjdGVyIHNldC5cXFwiXFxuXFxuICAgIChlbmZvcmNlXFxuICAgICAgKGlzLWNoYXJzZXQgQ09JTl9DSEFSU0VUIGFjY291bnQpXFxuICAgICAgKGZvcm1hdFxcbiAgICAgICAgXFxcIkFjY291bnQgZG9lcyBub3QgY29uZm9ybSB0byB0aGUgY29pbiBjb250cmFjdCBjaGFyc2V0OiB7fVxcXCJcXG4gICAgICAgIFthY2NvdW50XSkpXFxuXFxuICAgIChsZXQgKChhY2NvdW50LWxlbmd0aCAobGVuZ3RoIGFjY291bnQpKSlcXG5cXG4gICAgICAoZW5mb3JjZVxcbiAgICAgICAgKD49IGFjY291bnQtbGVuZ3RoIE1JTklNVU1fQUNDT1VOVF9MRU5HVEgpXFxuICAgICAgICAoZm9ybWF0XFxuICAgICAgICAgIFxcXCJBY2NvdW50IG5hbWUgZG9lcyBub3QgY29uZm9ybSB0byB0aGUgbWluIGxlbmd0aCByZXF1aXJlbWVudDoge31cXFwiXFxuICAgICAgICAgIFthY2NvdW50XSkpXFxuXFxuICAgICAgKGVuZm9yY2VcXG4gICAgICAgICg8PSBhY2NvdW50LWxlbmd0aCBNQVhJTVVNX0FDQ09VTlRfTEVOR1RIKVxcbiAgICAgICAgKGZvcm1hdFxcbiAgICAgICAgICBcXFwiQWNjb3VudCBuYW1lIGRvZXMgbm90IGNvbmZvcm0gdG8gdGhlIG1pbiBsZW5ndGggcmVxdWlyZW1lbnQ6IHt9XFxcIlxcbiAgICAgICAgICBbYWNjb3VudF0pKVxcbiAgICAgIClcXG4gIClcXG5cXG4gIDsgLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS1cXG4gIDsgQ29pbiBDb250cmFjdFxcblxcbiAgKGRlZnVuIGJ1eS1nYXM6c3RyaW5nIChzZW5kZXI6c3RyaW5nIHRvdGFsOmRlY2ltYWwpXFxuICAgIEBkb2MgXFxcIlRoaXMgZnVuY3Rpb24gZGVzY3JpYmVzIHRoZSBtYWluICdnYXMgYnV5JyBvcGVyYXRpb24uIEF0IHRoaXMgcG9pbnQgXFxcXFxcbiAgICBcXFxcTUlORVIgaGFzIGJlZW4gY2hvc2VuIGZyb20gdGhlIHBvb2wsIGFuZCB3aWxsIGJlIHZhbGlkYXRlZC4gVGhlIFNFTkRFUiAgIFxcXFxcXG4gICAgXFxcXG9mIHRoaXMgdHJhbnNhY3Rpb24gaGFzIHNwZWNpZmllZCBhIGdhcyBsaW1pdCBMSU1JVCAobWF4aW11bSBnYXMpIGZvciAgICBcXFxcXFxuICAgIFxcXFx0aGUgdHJhbnNhY3Rpb24sIGFuZCB0aGUgcHJpY2UgaXMgdGhlIHNwb3QgcHJpY2Ugb2YgZ2FzIGF0IHRoYXQgdGltZS4gICAgXFxcXFxcbiAgICBcXFxcVGhlIGdhcyBidXkgd2lsbCBiZSBleGVjdXRlZCBwcmlvciB0byBleGVjdXRpbmcgU0VOREVSJ3MgY29kZS5cXFwiXFxuXFxuICAgIEBtb2RlbCBbIChwcm9wZXJ0eSAoPiB0b3RhbCAwLjApKVxcbiAgICAgICAgICAgICAocHJvcGVydHkgKHZhbGlkLWFjY291bnQgc2VuZGVyKSlcXG4gICAgICAgICAgIF1cXG5cXG4gICAgKHZhbGlkYXRlLWFjY291bnQgc2VuZGVyKVxcblxcbiAgICAoZW5mb3JjZS11bml0IHRvdGFsKVxcbiAgICAoZW5mb3JjZSAoPiB0b3RhbCAwLjApIFxcXCJnYXMgc3VwcGx5IG11c3QgYmUgYSBwb3NpdGl2ZSBxdWFudGl0eVxcXCIpXFxuXFxuICAgIChyZXF1aXJlLWNhcGFiaWxpdHkgKEZVTkRfVFgpKVxcbiAgICAod2l0aC1jYXBhYmlsaXR5IChUUkFOU0ZFUilcXG4gICAgICAoZGViaXQgc2VuZGVyIHRvdGFsKSlcXG4gICAgKVxcblxcbiAgKGRlZnVuIHJlZGVlbS1nYXM6c3RyaW5nIChtaW5lcjpzdHJpbmcgbWluZXItZ3VhcmQ6Z3VhcmQgc2VuZGVyOnN0cmluZyB0b3RhbDpkZWNpbWFsKVxcbiAgICBAZG9jIFxcXCJUaGlzIGZ1bmN0aW9uIGRlc2NyaWJlcyB0aGUgbWFpbiAncmVkZWVtIGdhcycgb3BlcmF0aW9uLiBBdCB0aGlzICAgIFxcXFxcXG4gICAgXFxcXHBvaW50LCB0aGUgU0VOREVSJ3MgdHJhbnNhY3Rpb24gaGFzIGJlZW4gZXhlY3V0ZWQsIGFuZCB0aGUgZ2FzIHRoYXQgICAgICBcXFxcXFxuICAgIFxcXFx3YXMgY2hhcmdlZCBoYXMgYmVlbiBjYWxjdWxhdGVkLiBNSU5FUiB3aWxsIGJlIGNyZWRpdGVkIHRoZSBnYXMgY29zdCwgICAgXFxcXFxcbiAgICBcXFxcYW5kIFNFTkRFUiB3aWxsIHJlY2VpdmUgdGhlIHJlbWFpbmRlciB1cCB0byB0aGUgbGltaXRcXFwiXFxuXFxuICAgIEBtb2RlbCBbIChwcm9wZXJ0eSAoPiB0b3RhbCAwLjApKVxcbiAgICAgICAgICAgICAocHJvcGVydHkgKHZhbGlkLWFjY291bnQgc2VuZGVyKSlcXG4gICAgICAgICAgICAgKHByb3BlcnR5ICh2YWxpZC1hY2NvdW50IG1pbmVyKSlcXG4gICAgICAgICAgIF1cXG5cXG4gICAgKHZhbGlkYXRlLWFjY291bnQgc2VuZGVyKVxcbiAgICAodmFsaWRhdGUtYWNjb3VudCBtaW5lcilcXG4gICAgKGVuZm9yY2UtdW5pdCB0b3RhbClcXG5cXG4gICAgKHJlcXVpcmUtY2FwYWJpbGl0eSAoRlVORF9UWCkpXFxuICAgICh3aXRoLWNhcGFiaWxpdHkgKFRSQU5TRkVSKVxcbiAgICAgIChsZXQqICgoZmVlIChyZWFkLWRlY2ltYWwgXFxcImZlZVxcXCIpKVxcbiAgICAgICAgICAgICAocmVmdW5kICgtIHRvdGFsIGZlZSkpKVxcblxcbiAgICAgICAgKGVuZm9yY2UtdW5pdCBmZWUpXFxuXFxuICAgICAgICAoZW5mb3JjZSAoPj0gZmVlIDAuMClcXG4gICAgICAgICAgXFxcImZlZSBtdXN0IGJlIGEgbm9uLW5lZ2F0aXZlIHF1YW50aXR5XFxcIilcXG5cXG4gICAgICAgIChlbmZvcmNlICg-PSByZWZ1bmQgMC4wKVxcbiAgICAgICAgICBcXFwicmVmdW5kIG11c3QgYmUgYSBub24tbmVnYXRpdmUgcXVhbnRpdHlcXFwiKVxcblxcbiAgICAgICAgOyBkaXJlY3RseSB1cGRhdGUgaW5zdGVhZCBvZiBjcmVkaXRcXG4gICAgICAgIChpZiAoPiByZWZ1bmQgMC4wKVxcbiAgICAgICAgICAod2l0aC1yZWFkIGNvaW4tdGFibGUgc2VuZGVyXFxuICAgICAgICAgICAgeyBcXFwiYmFsYW5jZVxcXCIgOj0gYmFsYW5jZSB9XFxuICAgICAgICAgICAgKHVwZGF0ZSBjb2luLXRhYmxlIHNlbmRlclxcbiAgICAgICAgICAgICAgeyBcXFwiYmFsYW5jZVxcXCI6ICgrIGJhbGFuY2UgcmVmdW5kKSB9KVxcbiAgICAgICAgICAgIClcXG4gICAgICAgICAgXFxcIm5vb3BcXFwiKVxcblxcbiAgICAgICAgKGlmICg-IGZlZSAwLjApXFxuICAgICAgICAgIChjcmVkaXQgbWluZXIgbWluZXItZ3VhcmQgZmVlKVxcbiAgICAgICAgICBcXFwibm9vcFxcXCIpXFxuICAgICAgICApKVxcbiAgICApXFxuXFxuICAoZGVmdW4gY3JlYXRlLWFjY291bnQ6c3RyaW5nIChhY2NvdW50OnN0cmluZyBndWFyZDpndWFyZClcXG4gICAgQGRvYyBcXFwiQ3JlYXRlIGFuIGFjY291bnQgZm9yIEFDQ09VTlQsIHdpdGggR1VBUkQgY29udHJvbGxpbmcgYWNjZXNzIHRvIHRoZSAgXFxcXFxcbiAgICBcXFxcYWNjb3VudC5cXFwiXFxuXFxuICAgIEBtb2RlbCBbIChwcm9wZXJ0eSAodmFsaWQtYWNjb3VudCBhY2NvdW50KSkgXVxcblxcbiAgICAodmFsaWRhdGUtYWNjb3VudCBhY2NvdW50KVxcblxcbiAgICAoaW5zZXJ0IGNvaW4tdGFibGUgYWNjb3VudFxcbiAgICAgIHsgXFxcImJhbGFuY2VcXFwiIDogMC4wXFxuICAgICAgLCBcXFwiZ3VhcmRcXFwiICAgOiBndWFyZFxcbiAgICAgIH0pXFxuICAgIClcXG5cXG4gIChkZWZ1biBhY2NvdW50LWJhbGFuY2U6ZGVjaW1hbCAoYWNjb3VudDpzdHJpbmcpXFxuICAgIEBkb2MgXFxcIkNoZWNrIGFuIGFjY291bnQncyBiYWxhbmNlLlxcXCJcXG5cXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5ICh2YWxpZC1hY2NvdW50IGFjY291bnQpKSBdXFxuXFxuICAgICh2YWxpZGF0ZS1hY2NvdW50IGFjY291bnQpXFxuXFxuICAgICh3aXRoLXJlYWQgY29pbi10YWJsZSBhY2NvdW50XFxuICAgICAgeyBcXFwiYmFsYW5jZVxcXCIgOj0gYmFsYW5jZSB9XFxuICAgICAgYmFsYW5jZVxcbiAgICAgIClcXG4gICAgKVxcblxcbiAgKGRlZnVuIGFjY291bnQtaW5mbzpvYmplY3QgKGFjY291bnQ6c3RyaW5nKVxcbiAgICBAZG9jIFxcXCJHZXQgYWxsIG9mIGFuIGFjY291bnQncyBpbmZvLiAgVGhpcyBpbmNsdWRlcyB0aGUgYmFsYW5jZSBhbmQgdGhlICAgIFxcXFxcXG4gICAgXFxcXGd1YXJkLlxcXCJcXG5cXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5ICh2YWxpZC1hY2NvdW50IGFjY291bnQpKSBdXFxuXFxuICAgICh2YWxpZGF0ZS1hY2NvdW50IGFjY291bnQpXFxuXFxuICAgIChyZWFkIGNvaW4tdGFibGUgYWNjb3VudClcXG4gICAgKVxcblxcbiAgKGRlZnVuIHJvdGF0ZS1ndWFyZDpzdHJpbmcgKGFjY291bnQ6c3RyaW5nIG5ldy1ndWFyZDpndWFyZClcXG4gICAgQGRvYyBcXFwiUm90YXRlIGd1YXJkIGFzc29jaWF0ZWQgd2l0aCBBQ0NPVU5UXFxcIlxcblxcbiAgICBAbW9kZWwgWyAocHJvcGVydHkgKHZhbGlkLWFjY291bnQgYWNjb3VudCkpIF1cXG5cXG4gICAgKHZhbGlkYXRlLWFjY291bnQgYWNjb3VudClcXG5cXG4gICAgKHdpdGgtcmVhZCBjb2luLXRhYmxlIGFjY291bnRcXG4gICAgICB7IFxcXCJndWFyZFxcXCIgOj0gb2xkLWd1YXJkIH1cXG5cXG4gICAgICAoZW5mb3JjZS1ndWFyZCBvbGQtZ3VhcmQpXFxuXFxuICAgICAgKHVwZGF0ZSBjb2luLXRhYmxlIGFjY291bnRcXG4gICAgICAgIHsgXFxcImd1YXJkXFxcIiA6IG5ldy1ndWFyZCB9XFxuICAgICAgICApKSlcXG5cXG5cXG4gIChkZWZ1biB0cmFuc2ZlcjpzdHJpbmcgKHNlbmRlcjpzdHJpbmcgcmVjZWl2ZXI6c3RyaW5nIGFtb3VudDpkZWNpbWFsKVxcbiAgICBAZG9jIFxcXCJUcmFuc2ZlciBBTU9VTlQgYmV0d2VlbiBhY2NvdW50cyBTRU5ERVIgYW5kIFJFQ0VJVkVSIG9uIHRoZSBzYW1lICAgIFxcXFxcXG4gICAgXFxcXGNoYWluLiBUaGlzIGZhaWxzIGlmIGVpdGhlciBTRU5ERVIgb3IgUkVDRUlWRVIgZG9lcyBub3QgZXhpc3QuICAgICAgICAgICBcXFxcXFxuICAgIFxcXFxDcmVhdGUtb24tdHJhbnNmZXIgY2FuIGJlIGRvbmUgdXNpbmcgdGhlICd0cmFuc2Zlci1hbmQtY3JlYXRlJyBmdW5jdGlvbi5cXFwiXFxuXFxuICAgIEBtb2RlbCBbIChwcm9wZXJ0eSBjb25zZXJ2ZXMtbWFzcylcXG4gICAgICAgICAgICAgKHByb3BlcnR5ICg-IGFtb3VudCAwLjApKVxcbiAgICAgICAgICAgICAocHJvcGVydHkgKHZhbGlkLWFjY291bnQgc2VuZGVyKSlcXG4gICAgICAgICAgICAgKHByb3BlcnR5ICh2YWxpZC1hY2NvdW50IHJlY2VpdmVyKSlcXG4gICAgICAgICAgICAgKHByb3BlcnR5ICghPSBzZW5kZXIgcmVjZWl2ZXIpKSBdXFxuXFxuICAgIChlbmZvcmNlICghPSBzZW5kZXIgcmVjZWl2ZXIpXFxuICAgICAgXFxcInNlbmRlciBjYW5ub3QgYmUgdGhlIHJlY2VpdmVyIG9mIGEgdHJhbnNmZXJcXFwiKVxcblxcbiAgICAodmFsaWRhdGUtYWNjb3VudCBzZW5kZXIpXFxuICAgICh2YWxpZGF0ZS1hY2NvdW50IHJlY2VpdmVyKVxcblxcbiAgICAoZW5mb3JjZSAoPiBhbW91bnQgMC4wKVxcbiAgICAgIFxcXCJ0cmFuc2ZlciBhbW91bnQgbXVzdCBiZSBwb3NpdGl2ZVxcXCIpXFxuXFxuICAgIChlbmZvcmNlLXVuaXQgYW1vdW50KVxcblxcbiAgICAod2l0aC1jYXBhYmlsaXR5IChUUkFOU0ZFUilcXG4gICAgICAoZGViaXQgc2VuZGVyIGFtb3VudClcXG4gICAgICAod2l0aC1yZWFkIGNvaW4tdGFibGUgcmVjZWl2ZXJcXG4gICAgICAgIHsgXFxcImd1YXJkXFxcIiA6PSBnIH1cXG5cXG4gICAgICAgIChjcmVkaXQgcmVjZWl2ZXIgZyBhbW91bnQpKVxcbiAgICAgIClcXG4gICAgKVxcblxcbiAgKGRlZnVuIHRyYW5zZmVyLWNyZWF0ZTpzdHJpbmdcXG4gICAgKCBzZW5kZXI6c3RyaW5nXFxuICAgICAgcmVjZWl2ZXI6c3RyaW5nXFxuICAgICAgcmVjZWl2ZXItZ3VhcmQ6Z3VhcmRcXG4gICAgICBhbW91bnQ6ZGVjaW1hbCApXFxuXFxuICAgIEBkb2MgXFxcIlRyYW5zZmVyIGJldHdlZW4gYWNjb3VudHMgU0VOREVSIGFuZCBSRUNFSVZFUiBvbiB0aGUgc2FtZSBjaGFpbi4gICAgXFxcXFxcbiAgICBcXFxcVGhpcyBmYWlscyBpZiB0aGUgU0VOREVSIGFjY291bnQgZG9lcyBub3QgZXhpc3QuIElmIHRoZSBSRUNFSVZFUiBhY2NvdW50IFxcXFxcXG4gICAgXFxcXGRvZXMgbm90IGV4aXN0LCBpdCBpcyBjcmVhdGVkIGFuZCBhc3NvY2lhdGVkIHdpdGggR1VBUkQuXFxcIlxcblxcbiAgICBAbW9kZWwgWyA7KHByb3BlcnR5IGNvbnNlcnZlcy1tYXNzKSA7OyBmYWlscyBvbiBtaXNzaW5nIHJvdywgRlYgcHJvYmxlbVxcbiAgICAgICAgICAgIChwcm9wZXJ0eSAoPiBhbW91bnQgMC4wKSlcXG4gICAgICAgICAgICAocHJvcGVydHkgKHZhbGlkLWFjY291bnQgc2VuZGVyKSlcXG4gICAgICAgICAgICAocHJvcGVydHkgKHZhbGlkLWFjY291bnQgcmVjZWl2ZXIpKVxcbiAgICAgICAgICAgIChwcm9wZXJ0eSAoIT0gc2VuZGVyIHJlY2VpdmVyKSkgXVxcblxcbiAgICAoZW5mb3JjZSAoIT0gc2VuZGVyIHJlY2VpdmVyKVxcbiAgICAgIFxcXCJzZW5kZXIgY2Fubm90IGJlIHRoZSByZWNlaXZlciBvZiBhIHRyYW5zZmVyXFxcIilcXG5cXG4gICAgKHZhbGlkYXRlLWFjY291bnQgc2VuZGVyKVxcbiAgICAodmFsaWRhdGUtYWNjb3VudCByZWNlaXZlcilcXG5cXG4gICAgKGVuZm9yY2UgKD4gYW1vdW50IDAuMClcXG4gICAgICBcXFwidHJhbnNmZXIgYW1vdW50IG11c3QgYmUgcG9zaXRpdmVcXFwiKVxcblxcbiAgICAoZW5mb3JjZS11bml0IGFtb3VudClcXG5cXG4gICAgKHdpdGgtY2FwYWJpbGl0eSAoVFJBTlNGRVIpXFxuICAgICAgKGRlYml0IHNlbmRlciBhbW91bnQpXFxuICAgICAgKGNyZWRpdCByZWNlaXZlciByZWNlaXZlci1ndWFyZCBhbW91bnQpKVxcbiAgICApXFxuXFxuICAoZGVmdW4gY29pbmJhc2U6c3RyaW5nIChhY2NvdW50OnN0cmluZyBhY2NvdW50LWd1YXJkOmd1YXJkIGFtb3VudDpkZWNpbWFsKVxcbiAgICBAZG9jIFxcXCJJbnRlcm5hbCBmdW5jdGlvbiBmb3IgdGhlIGluaXRpYWwgY3JlYXRpb24gb2YgY29pbnMuICBUaGlzIGZ1bmN0aW9uIFxcXFxcXG4gICAgXFxcXGNhbm5vdCBiZSB1c2VkIG91dHNpZGUgb2YgdGhlIGNvaW4gY29udHJhY3QuXFxcIlxcblxcbiAgICBAbW9kZWwgWyAocHJvcGVydHkgKHZhbGlkLWFjY291bnQgYWNjb3VudCkpIF1cXG5cXG4gICAgKHZhbGlkYXRlLWFjY291bnQgYWNjb3VudClcXG4gICAgKGVuZm9yY2UtdW5pdCBhbW91bnQpXFxuXFxuICAgIChyZXF1aXJlLWNhcGFiaWxpdHkgKENPSU5CQVNFKSlcXG4gICAgKHdpdGgtY2FwYWJpbGl0eSAoVFJBTlNGRVIpXFxuICAgICAgKGNyZWRpdCBhY2NvdW50IGFjY291bnQtZ3VhcmQgYW1vdW50KSlcXG4gICAgKVxcblxcbiAgKGRlZnBhY3QgZnVuZC10eCAoc2VuZGVyOnN0cmluZyBtaW5lcjpzdHJpbmcgbWluZXItZ3VhcmQ6Z3VhcmQgdG90YWw6ZGVjaW1hbClcXG4gICAgQGRvYyBcXFwiJ2Z1bmQtdHgnIGlzIGEgc3BlY2lhbCBwYWN0IHRvIGZ1bmQgYSB0cmFuc2FjdGlvbiBpbiB0d28gc3RlcHMsICAgICBcXFxcXFxuICAgIFxcXFx3aXRoIHRoZSBhY3R1YWwgdHJhbnNhY3Rpb24gdHJhbnNwaXJpbmcgaW4gdGhlIG1pZGRsZTogICAgICAgICAgICAgICAgICAgXFxcXFxcbiAgICBcXFxcICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFxcXFxcXG4gICAgXFxcXCAgMSkgQSBidXlpbmcgcGhhc2UsIGRlYml0aW5nIHRoZSBzZW5kZXIgZm9yIHRvdGFsIGdhcyBhbmQgZmVlLCB5aWVsZGluZyBcXFxcXFxuICAgIFxcXFwgICAgIFRYX01BWF9DSEFSR0UuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXFxcXFxcbiAgICBcXFxcICAyKSBBIHNldHRsZW1lbnQgcGhhc2UsIHJlc3VtaW5nIFRYX01BWF9DSEFSR0UsIGFuZCBhbGxvY2F0aW5nIHRvIHRoZSAgIFxcXFxcXG4gICAgXFxcXCAgICAgY29pbmJhc2UgYWNjb3VudCBmb3IgdXNlZCBnYXMgYW5kIGZlZSwgYW5kIHNlbmRlciBhY2NvdW50IGZvciBiYWwtICBcXFxcXFxuICAgIFxcXFwgICAgIGFuY2UgKHVudXNlZCBnYXMsIGlmIGFueSkuXFxcIlxcblxcbiAgICBAbW9kZWwgWyAocHJvcGVydHkgKD4gdG90YWwgMC4wKSlcXG4gICAgICAgICAgICAgKHByb3BlcnR5ICh2YWxpZC1hY2NvdW50IHNlbmRlcikpXFxuICAgICAgICAgICAgIChwcm9wZXJ0eSAodmFsaWQtYWNjb3VudCBtaW5lcikpXFxuICAgICAgICAgICAgIDsocHJvcGVydHkgY29uc2VydmVzLW1hc3MpIG5vdCBzdXBwb3J0ZWQgeWV0XFxuICAgICAgICAgICBdXFxuXFxuICAgIChzdGVwIChidXktZ2FzIHNlbmRlciB0b3RhbCkpXFxuICAgIChzdGVwIChyZWRlZW0tZ2FzIG1pbmVyIG1pbmVyLWd1YXJkIHNlbmRlciB0b3RhbCkpXFxuICAgIClcXG5cXG4gIChkZWZ1biBkZWJpdDpzdHJpbmcgKGFjY291bnQ6c3RyaW5nIGFtb3VudDpkZWNpbWFsKVxcbiAgICBAZG9jIFxcXCJEZWJpdCBBTU9VTlQgZnJvbSBBQ0NPVU5UIGJhbGFuY2VcXFwiXFxuXFxuICAgIEBtb2RlbCBbIChwcm9wZXJ0eSAoPiBhbW91bnQgMC4wKSlcXG4gICAgICAgICAgICAgKHByb3BlcnR5ICh2YWxpZC1hY2NvdW50IGFjY291bnQpKVxcbiAgICAgICAgICAgXVxcblxcbiAgICAodmFsaWRhdGUtYWNjb3VudCBhY2NvdW50KVxcblxcbiAgICAoZW5mb3JjZSAoPiBhbW91bnQgMC4wKVxcbiAgICAgIFxcXCJkZWJpdCBhbW91bnQgbXVzdCBiZSBwb3NpdGl2ZVxcXCIpXFxuXFxuICAgIChlbmZvcmNlLXVuaXQgYW1vdW50KVxcblxcbiAgICAocmVxdWlyZS1jYXBhYmlsaXR5IChUUkFOU0ZFUikpXFxuICAgICh3aXRoLWNhcGFiaWxpdHkgKEFDQ09VTlRfR1VBUkQgYWNjb3VudClcXG4gICAgICAod2l0aC1yZWFkIGNvaW4tdGFibGUgYWNjb3VudFxcbiAgICAgICAgeyBcXFwiYmFsYW5jZVxcXCIgOj0gYmFsYW5jZSB9XFxuXFxuICAgICAgICAoZW5mb3JjZSAoPD0gYW1vdW50IGJhbGFuY2UpIFxcXCJJbnN1ZmZpY2llbnQgZnVuZHNcXFwiKVxcbiAgICAgICAgKHVwZGF0ZSBjb2luLXRhYmxlIGFjY291bnRcXG4gICAgICAgICAgeyBcXFwiYmFsYW5jZVxcXCIgOiAoLSBiYWxhbmNlIGFtb3VudCkgfVxcbiAgICAgICAgICApKSlcXG4gICAgKVxcblxcblxcbiAgKGRlZnVuIGNyZWRpdDpzdHJpbmcgKGFjY291bnQ6c3RyaW5nIGd1YXJkOmd1YXJkIGFtb3VudDpkZWNpbWFsKVxcbiAgICBAZG9jIFxcXCJDcmVkaXQgQU1PVU5UIHRvIEFDQ09VTlQgYmFsYW5jZVxcXCJcXG5cXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5ICg-IGFtb3VudCAwLjApKVxcbiAgICAgICAgICAgICAocHJvcGVydHkgKHZhbGlkLWFjY291bnQgYWNjb3VudCkpXFxuICAgICAgICAgICBdXFxuXFxuICAgICh2YWxpZGF0ZS1hY2NvdW50IGFjY291bnQpXFxuXFxuICAgIChlbmZvcmNlICg-IGFtb3VudCAwLjApIFxcXCJjcmVkaXQgYW1vdW50IG11c3QgYmUgcG9zaXRpdmVcXFwiKVxcbiAgICAoZW5mb3JjZS11bml0IGFtb3VudClcXG5cXG4gICAgKHJlcXVpcmUtY2FwYWJpbGl0eSAoVFJBTlNGRVIpKVxcbiAgICAod2l0aC1kZWZhdWx0LXJlYWQgY29pbi10YWJsZSBhY2NvdW50XFxuICAgICAgeyBcXFwiYmFsYW5jZVxcXCIgOiAwLjAsIFxcXCJndWFyZFxcXCIgOiBndWFyZCB9XFxuICAgICAgeyBcXFwiYmFsYW5jZVxcXCIgOj0gYmFsYW5jZSwgXFxcImd1YXJkXFxcIiA6PSByZXRnIH1cXG4gICAgICA7IHdlIGRvbid0IHdhbnQgdG8gb3ZlcndyaXRlIGFuIGV4aXN0aW5nIGd1YXJkIHdpdGggdGhlIHVzZXItc3VwcGxpZWQgb25lXFxuICAgICAgKGVuZm9yY2UgKD0gcmV0ZyBndWFyZClcXG4gICAgICAgIFxcXCJhY2NvdW50IGd1YXJkcyBkbyBub3QgbWF0Y2hcXFwiKVxcblxcbiAgICAgICh3cml0ZSBjb2luLXRhYmxlIGFjY291bnRcXG4gICAgICAgIHsgXFxcImJhbGFuY2VcXFwiIDogKCsgYmFsYW5jZSBhbW91bnQpXFxuICAgICAgICAsIFxcXCJndWFyZFxcXCIgICA6IHJldGdcXG4gICAgICAgIH0pXFxuICAgICAgKSlcXG5cXG4gIChkZWZwYWN0IGNyb3NzLWNoYWluLXRyYW5zZmVyXFxuICAgICggZGVsZXRlLWFjY291bnQ6c3RyaW5nXFxuICAgICAgY3JlYXRlLWNoYWluLWlkOnN0cmluZ1xcbiAgICAgIGNyZWF0ZS1hY2NvdW50OnN0cmluZ1xcbiAgICAgIGNyZWF0ZS1hY2NvdW50LWd1YXJkOmd1YXJkXFxuICAgICAgcXVhbnRpdHk6ZGVjaW1hbCApXFxuXFxuICAgIEBkb2MgXFxcIlRyYW5zZmVyIFFVQU5USVRZIGNvaW5zIGZyb20gREVMRVRFLUFDQ09VTlQgb24gY3VycmVudCBjaGFpbiB0byAgICAgICAgICAgXFxcXFxcbiAgICAgICAgIFxcXFxDUkVBVEUtQUNDT1VOVCBvbiBDUkVBVEUtQ0hBSU4tSUQuIFRhcmdldCBjaGFpbiBpZCBtdXN0IG5vdCBiZSB0aGUgICAgICAgIFxcXFxcXG4gICAgICAgICBcXFxcY3VycmVudCBjaGFpbi1pZC4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBcXFxcXFxuICAgICAgICAgXFxcXCAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXFxcXFxcbiAgICAgICAgIFxcXFxTdGVwIDE6IEJ1cm4gUVVBTlRJVFktbWFueSBjb2lucyBmb3IgREVMRVRFLUFDQ09VTlQgb24gdGhlIGN1cnJlbnQgY2hhaW4sIFxcXFxcXG4gICAgICAgICBcXFxcYW5kIHByb2R1Y2UgYW4gU1BWIHJlY2VpcHQgd2hpY2ggbWF5IGJlIG1hbnVhbGx5IHJlZGVlbWVkIGZvciBhbiBTUFYgICAgICBcXFxcXFxuICAgICAgICAgXFxcXHByb29mLiBPbmNlIGEgcHJvb2YgaXMgb2J0YWluZWQsIHRoZSB1c2VyIG1heSBjYWxsICdjcmVhdGUtY29pbicgYW5kICAgICAgXFxcXFxcbiAgICAgICAgIFxcXFxjb25zdW1lIHRoZSBwcm9vZiBvbiBDUkVBVEUtQ0hBSU4tSUQsIGNyZWRpdGluZyBDUkVBVEUtQUNDT1VOVCBRVUFOVElUWS0gIFxcXFxcXG4gICAgICAgICBcXFxcbWFueSBjb2lucy4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBcXFxcXFxuICAgICAgICAgXFxcXCAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXFxcXFxcbiAgICAgICAgIFxcXFxTdGVwIDI6IENvbnN1bWUgYW4gU1BWIHByb29mIGZvciBhIG51bWJlciBvZiBjb2lucywgYW5kIGNyZWRpdCB0aGUgICAgICAgIFxcXFxcXG4gICAgICAgICBcXFxcYWNjb3VudCBhc3NvY2lhdGVkIHdpdGggdGhlIHByb29mIHRoZSBxdWFudGlmeSBvZiBjb2lucyBidXJuZWQgb24gdGhlICAgICBcXFxcXFxuICAgICAgICAgXFxcXHNvdXJjZSBjaGFpbiBieSB0aGUgYnVybiBhY2NvdW50LiBOb3RlOiBtdXN0IGJlIGNhbGxlZCBvbiB0aGUgY29ycmVjdCAgICAgXFxcXFxcbiAgICAgICAgIFxcXFxjaGFpbiBpZCBhcyBzcGVjaWZpZWQgaW4gdGhlIHByb29mLlxcXCJcXG5cXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5ICg-IHF1YW50aXR5IDAuMCkpXFxuICAgICAgICAgICAgIChwcm9wZXJ0eSAoIT0gY3JlYXRlLWNoYWluLWlkIFxcXCJcXFwiKSlcXG4gICAgICAgICAgICAgKHByb3BlcnR5ICh2YWxpZC1hY2NvdW50IGRlbGV0ZS1hY2NvdW50KSlcXG4gICAgICAgICAgICAgKHByb3BlcnR5ICh2YWxpZC1hY2NvdW50IGNyZWF0ZS1hY2NvdW50KSlcXG4gICAgICAgICAgIF1cXG5cXG4gICAgKHN0ZXBcXG4gICAgICAod2l0aC1jYXBhYmlsaXR5IChUUkFOU0ZFUilcXG5cXG4gICAgICAgICh2YWxpZGF0ZS1hY2NvdW50IGRlbGV0ZS1hY2NvdW50KVxcbiAgICAgICAgKHZhbGlkYXRlLWFjY291bnQgY3JlYXRlLWFjY291bnQpXFxuXFxuICAgICAgICAoZW5mb3JjZSAoIT0gXFxcIlxcXCIgY3JlYXRlLWNoYWluLWlkKSBcXFwiZW1wdHkgY3JlYXRlLWNoYWluLWlkXFxcIilcXG4gICAgICAgIChlbmZvcmNlICghPSAoYXQgJ2NoYWluLWlkIChjaGFpbi1kYXRhKSkgY3JlYXRlLWNoYWluLWlkKVxcbiAgICAgICAgICBcXFwiY2Fubm90IHJ1biBjcm9zcy1jaGFpbiB0cmFuc2ZlcnMgdG8gdGhlIHNhbWUgY2hhaW5cXFwiKVxcblxcbiAgICAgICAgKGVuZm9yY2UgKD4gcXVhbnRpdHkgMC4wKVxcbiAgICAgICAgICBcXFwidHJhbnNmZXIgcXVhbnRpdHkgbXVzdCBiZSBwb3NpdGl2ZVxcXCIpXFxuXFxuICAgICAgICAoZW5mb3JjZS11bml0IHF1YW50aXR5KVxcblxcbiAgICAgICAgOzsgc3RlcCAxIC0gZGViaXQgZGVsZXRlLWFjY291bnQgb24gY3VycmVudCBjaGFpblxcbiAgICAgICAgKGRlYml0IGRlbGV0ZS1hY2NvdW50IHF1YW50aXR5KVxcblxcbiAgICAgICAgKGxldFxcbiAgICAgICAgICAoKHJldHY6b2JqZWN0e3RyYW5zZmVyLXNjaGVtYX1cXG4gICAgICAgICAgICB7IFxcXCJjcmVhdGUtYWNjb3VudFxcXCIgOiBjcmVhdGUtYWNjb3VudFxcbiAgICAgICAgICAgICwgXFxcImNyZWF0ZS1hY2NvdW50LWd1YXJkXFxcIiA6IGNyZWF0ZS1hY2NvdW50LWd1YXJkXFxuICAgICAgICAgICAgLCBcXFwicXVhbnRpdHlcXFwiIDogcXVhbnRpdHlcXG4gICAgICAgICAgICB9KSlcXG4gICAgICAgICAgKHlpZWxkIHJldHYgY3JlYXRlLWNoYWluLWlkKVxcbiAgICAgICAgICApKSlcXG5cXG4gICAgKHN0ZXBcXG4gICAgICAocmVzdW1lXFxuICAgICAgICB7IFxcXCJjcmVhdGUtYWNjb3VudFxcXCIgOj0gY3JlYXRlLWFjY291bnRcXG4gICAgICAgICwgXFxcImNyZWF0ZS1hY2NvdW50LWd1YXJkXFxcIiA6PSBjcmVhdGUtYWNjb3VudC1ndWFyZFxcbiAgICAgICAgLCBcXFwicXVhbnRpdHlcXFwiIDo9IHF1YW50aXR5XFxuICAgICAgICB9XFxuXFxuICAgICAgICA7OyBzdGVwIDIgLSBjcmVkaXQgY3JlYXRlIGFjY291bnQgb24gdGFyZ2V0IGNoYWluXFxuICAgICAgICAod2l0aC1jYXBhYmlsaXR5IChUUkFOU0ZFUilcXG4gICAgICAgICAgKGNyZWRpdCBjcmVhdGUtYWNjb3VudCBjcmVhdGUtYWNjb3VudC1ndWFyZCBxdWFudGl0eSkpXFxuICAgICAgICApKVxcbiAgICApXFxuXFxuXFxuICA7IC0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tXFxuICA7IENvaW4gYWxsb2NhdGlvbnNcXG5cXG4gIChkZWZzY2hlbWEgYWxsb2NhdGlvbi1zY2hlbWFcXG4gICAgQGRvYyBcXFwiVGhlIGNvaW4gYWxsb2NhdGlvbiBzY2hlbWEgZm9yIGdlbmVzaXMgbG9ja3Vwc1xcXCJcXG4gICAgO0Btb2RlbCBbIChpbnZhcmlhbnQgKD49IGJhbGFuY2UgMC4wKSkgXVxcblxcbiAgICBiYWxhbmNlOmRlY2ltYWxcXG4gICAgZGF0ZTp0aW1lXFxuICAgIGd1YXJkOmd1YXJkXFxuICAgIHJlZGVlbWVkOmJvb2wpXFxuXFxuICAoZGVmdGFibGUgYWxsb2NhdGlvbi10YWJsZTp7YWxsb2NhdGlvbi1zY2hlbWF9KVxcblxcbiAgKGRlZnVuIGNyZWF0ZS1hbGxvY2F0aW9uLWFjY291bnRcXG4gICAgICAoIGFjY291bnQ6c3RyaW5nXFxuICAgICAgICBkYXRlOnRpbWVcXG4gICAgICAgIGd1YXJkLXJlZjpzdHJpbmdcXG4gICAgICAgIGFtb3VudDpkZWNpbWFsXFxuICAgICAgKVxcblxcbiAgICBAZG9jIFxcXCJBZGQgYW4gZW50cnkgdG8gdGhlIGNvaW4gYWxsb2NhdGlvbiB0YWJsZS4gUmVxdWlyZXMgQUxMT0NBVElPTlxcXCJcXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5ICh2YWxpZC1hY2NvdW50IGFjY291bnQpKSBdXFxuXFxuICAgIChyZXF1aXJlLWNhcGFiaWxpdHkgKEdFTkVTSVMpKVxcblxcbiAgICAodmFsaWRhdGUtYWNjb3VudCBhY2NvdW50KVxcbiAgICAoZW5mb3JjZSAoPj0gYW1vdW50IDAuMClcXG4gICAgICBcXFwiYWxsb2NhdGlvbiBhbW91bnQgbXVzdCBiZSBub24tbmVnYXRpdmVcXFwiKVxcblxcbiAgICAoZW5mb3JjZS11bml0IGFtb3VudClcXG5cXG4gICAgKGxldFxcbiAgICAgICgoZ3VhcmQ6Z3VhcmQgKGtleXNldC1yZWYtZ3VhcmQgZ3VhcmQtcmVmKSkpXFxuXFxuICAgICAgKGNyZWF0ZS1hY2NvdW50IGFjY291bnQgZ3VhcmQpXFxuICAgICAgKGluc2VydCBhbGxvY2F0aW9uLXRhYmxlIGFjY291bnRcXG4gICAgICAgIHsgXFxcImJhbGFuY2VcXFwiIDogYW1vdW50XFxuICAgICAgICAsIFxcXCJkYXRlXFxcIiA6IGRhdGVcXG4gICAgICAgICwgXFxcImd1YXJkXFxcIiA6IChrZXlzZXQtcmVmLWd1YXJkIGd1YXJkLXJlZilcXG4gICAgICAgICwgXFxcInJlZGVlbWVkXFxcIiA6IGZhbHNlXFxuICAgICAgICB9KSkpXFxuXFxuICAoZGVmdW4gcmVsZWFzZS1hbGxvY2F0aW9uXFxuICAgICggYWNjb3VudDpzdHJpbmcgKVxcblxcbiAgICBAZG9jIFxcXCJSZWxlYXNlIGZ1bmRzIGFzc29jaWF0ZWQgd2l0aCBhbiBhbGxvY2F0aW9uIGFjY291bnRcXFwiXFxuICAgIEBtb2RlbFxcbiAgICAgIFsgKHByb3BlcnR5ICh2YWxpZC1hY2NvdW50IGFjY291bnQpKSBdXFxuXFxuICAgICh2YWxpZGF0ZS1hY2NvdW50IGFjY291bnQpXFxuXFxuICAgICh3aXRoLXJlYWQgYWxsb2NhdGlvbi10YWJsZSBhY2NvdW50XFxuICAgICAgeyBcXFwiYmFsYW5jZVxcXCIgOj0gYmFsYW5jZVxcbiAgICAgICwgXFxcImRhdGVcXFwiIDo9IHJlbGVhc2UtdGltZVxcbiAgICAgICwgXFxcInJlZGVlbWVkXFxcIiA6PSByZWRlZW1lZFxcbiAgICAgICwgXFxcImd1YXJkXFxcIiA6PSBndWFyZFxcbiAgICAgIH1cXG5cXG4gICAgICAobGV0ICgoY3Vyci10aW1lOnRpbWUgKGF0ICdibG9jay10aW1lIChjaGFpbi1kYXRhKSkpKVxcblxcbiAgICAgICAgKGVuZm9yY2UgKG5vdCByZWRlZW1lZClcXG4gICAgICAgICAgXFxcImFsbG9jYXRpb24gZnVuZHMgaGF2ZSBhbHJlYWR5IGJlZW4gcmVkZWVtZWRcXFwiKVxcblxcbiAgICAgICAgKGVuZm9yY2VcXG4gICAgICAgICAgKD49IChkaWZmLXRpbWUgcmVsZWFzZS10aW1lIGN1cnItdGltZSkgMC4wKVxcbiAgICAgICAgICAoZm9ybWF0IFxcXCJmdW5kcyBsb2NrZWQgdW50aWwge31cXFwiIFtyZWxlYXNlLXRpbWVdKSlcXG5cXG4gICAgICAgIChlbmZvcmNlLWd1YXJkIGd1YXJkKVxcblxcbiAgICAgICAgKHdpdGgtY2FwYWJpbGl0eSAoVFJBTlNGRVIpXFxuICAgICAgICAgIDsgcmVsZWFzZSBmdW5kcyB2aWEgY29pbmJhc2UgdG8gYWNjb3VudFxcbiAgICAgICAgICAoY3JlZGl0IGFjY291bnQgZ3VhcmQgYmFsYW5jZSlcXG5cXG4gICAgICAgICAgKHVwZGF0ZSBhbGxvY2F0aW9uLXRhYmxlIGFjY291bnRcXG4gICAgICAgICAgICB7IFxcXCJyZWRlZW1lZFxcXCIgOiB0cnVlXFxuICAgICAgICAgICAgLCBcXFwiYmFsYW5jZVxcXCIgOiAwLjBcXG4gICAgICAgICAgICB9KVxcblxcbiAgICAgICAgICBcXFwiQWxsb2NhdGlvbiBzdWNjZXNzZnVsXFxcIilcXG4gICAgKSkpXFxuXFxuKVxcblxcbihjcmVhdGUtdGFibGUgY29pbi10YWJsZSlcXG4oY3JlYXRlLXRhYmxlIGFsbG9jYXRpb24tdGFibGUpXFxuXCJ9fSxcInNpZ25lcnNcIjpbXSxcIm1ldGFcIjp7XCJjcmVhdGlvblRpbWVcIjowLFwidHRsXCI6MTcyODAwLFwiZ2FzTGltaXRcIjowLFwiY2hhaW5JZFwiOlwiXCIsXCJnYXNQcmljZVwiOjAsXCJzZW5kZXJcIjpcIlwifSxcIm5vbmNlXCI6XCJcXFwiZ2VuZXNpcy0wMVxcXCJcIn0ifQ
  - eyJnYXMiOjAsInJlc3VsdCI6eyJzdGF0dXMiOiJzdWNjZXNzIiwiZGF0YSI6IlRhYmxlQ3JlYXRlZCJ9LCJyZXFLZXkiOiJQcXZERUQxV1VpSEY2azg2ZjZBR2tZUGNmbmFwQ0tuZnNEMWI0TTdVMVVBIiwibG9ncyI6InhEZU5hOXFCcXR2eTg2LXZBcEZxZmtoNVF4em8yNFljMy03bUVLeHBIQnMiLCJtZXRhRGF0YSI6bnVsbCwiY29udGludWF0aW9uIjpudWxsLCJ0eElkIjowfQ
- - eyJoYXNoIjoiLVJkZzVidlB1bjlvTlYtQ2l2OWNqLUE3YVhfRVhMczlSQVNSdFRPb2djVSIsInNpZ3MiOlt7InNpZyI6Ijc5ZGMwMjg2NzFjOTVlZjQ2NmJkYTQ3ZGVmNmM0OGE2YWJmNWFjMDExZDA2MzM3MzFlYzFjNmFlYTc5NzFiOTZlY2NlZDkyOGQ1NTk2ZTZiNGIzNWI2MDdhZDNkYWQzMzAzZmMyZjg4ZjA3YzRmZDFiNDdiYmVhYzZjYzIzYTA4In1dLCJjbWQiOiJ7XCJuZXR3b3JrSWRcIjpudWxsLFwicGF5bG9hZFwiOntcImV4ZWNcIjp7XCJkYXRhXCI6e1wibnMtYWRtaW4ta2V5c2V0XCI6W1wiMzY4ODIwZjgwYzMyNGJiYzdjMmIwNjEwNjg4YTdkYTQzZTM5ZjkxZDExODczMjY3MWNkOWM3NTAwZmY0M2NjYVwiXSxcIm5zLW9wZXJhdGUta2V5c2V0XCI6W1wiMzY4ODIwZjgwYzMyNGJiYzdjMmIwNjEwNjg4YTdkYTQzZTM5ZjkxZDExODczMjY3MWNkOWM3NTAwZmY0M2NjYVwiXX0sXCJjb2RlXCI6XCJcXG4oZGVmaW5lLWtleXNldCAnbnMtYWRtaW4ta2V5c2V0IChyZWFkLWtleXNldCAnbnMtYWRtaW4ta2V5c2V0KSlcXG4oZGVmaW5lLWtleXNldCAnbnMtb3BlcmF0ZS1rZXlzZXQgKHJlYWQta2V5c2V0ICducy1vcGVyYXRlLWtleXNldCkpXFxuXFxuKG1vZHVsZSBucyAnbnMtYWRtaW4ta2V5c2V0XFxuICBcXFwiQWRtaW5pc3RlcnMgZGVmaW5pdGlvbiBvZiBuZXcgbmFtZXNwYWNlcyBpbiBDaGFpbndlYi5cXFwiXFxuXFxuICAoZGVmc2NoZW1hIHJlZy1lbnRyeVxcbiAgICBhZG1pbi1ndWFyZDpndWFyZFxcbiAgICBhY3RpdmU6Ym9vbClcXG5cXG4gIChkZWZ0YWJsZSByZWdpc3RyeTp7cmVnLWVudHJ5fSlcXG5cXG4gIChkZWZjYXAgT1BFUkFURSAoKVxcbiAgICAoZW5mb3JjZS1rZXlzZXQgJ25zLW9wZXJhdGUta2V5c2V0KSlcXG5cXG4gIChkZWZjb25zdCBHVUFSRF9TVUNDRVNTIChjcmVhdGUtdXNlci1ndWFyZCAoc3VjY2VzcykpKVxcbiAgKGRlZmNvbnN0IEdVQVJEX0ZBSUxVUkUgKGNyZWF0ZS11c2VyLWd1YXJkIChmYWlsdXJlKSkpXFxuXFxuICAoZGVmdW4gc3VjY2VzcyAoKVxcbiAgICB0cnVlKVxcbiAgKGRlZnVuIGZhaWx1cmUgKClcXG4gICAgKGVuZm9yY2UgZmFsc2UgXFxcIkRpc2FibGVkXFxcIikpXFxuXFxuICAoZGVmdW4gdmFsaWRhdGUtbmFtZSAobmFtZSlcXG4gICAgKGVuZm9yY2UgKCE9IFxcXCJcXFwiIG5hbWUpIFxcXCJFbXB0eSBuYW1lIG5vdCBhbGxvd2VkXFxcIilcXG4gICAgKGVuZm9yY2UgKDwgKGxlbmd0aCBuYW1lKSA2NCkgXFxcIk5hbWUgbXVzdCBiZSBsZXNzIHRoYW4gNjQgY2hhcmFjdGVycyBsb25nXFxcIilcXG4gICAgKGVuZm9yY2UgKGlzLWNoYXJzZXQgQ0hBUlNFVF9MQVRJTjEgbmFtZSlcXG4gICAgICAgICAgICAgXFxcIk5hbWUgbXVzdCBiZSBpbiBsYXRpbjEgY2hhcnNldFxcXCIpKVxcblxcbiAgKGRlZnVuIHZhbGlkYXRlOmJvb2xcXG4gICAgICAoIG5zLW5hbWU6c3RyaW5nXFxuICAgICAgICBucy1hZG1pbjpndWFyZFxcbiAgICAgICAgKVxcbiAgICBcXFwiIE1hbmFnZXMgbmFtZXNwYWNlIGluc3RhbGwgZm9yIENoYWlud2ViLiBSZXF1aXJlcyBhY3RpdmUgcm93IGluIHJlZ2lzdHJ5IFxcXFxcXG4gICAgXFxcXCBmb3IgTlMtTkFNRSB3aXRoIGd1YXJkIG1hdGNoaW5nIE5TLUFETUlOLlxcXCJcXG5cXG4gICAgKHZhbGlkYXRlLW5hbWUgbnMtbmFtZSlcXG5cXG4gICAgKHdpdGgtZGVmYXVsdC1yZWFkIHJlZ2lzdHJ5IG5zLW5hbWVcXG4gICAgICB7ICdhZG1pbi1ndWFyZCA6IG5zLWFkbWluXFxuICAgICAgLCAnYWN0aXZlIDogZmFsc2UgfVxcbiAgICAgIHsgJ2FkbWluLWd1YXJkIDo9IGFnXFxuICAgICAgLCAnYWN0aXZlIDo9IGlzLWFjdGl2ZSB9XFxuXFxuICAgICAgICAoZW5mb3JjZSBpcy1hY3RpdmUgXFxcIkluYWN0aXZlIG9yIHVucmVnaXN0ZXJlZCBuYW1lc3BhY2VcXFwiKVxcbiAgICAgICAgKGVuZm9yY2UgKD0gbnMtYWRtaW4gYWcpIFxcXCJBZG1pbiBndWFyZCBtdXN0IG1hdGNoIGd1YXJkIGluIHJlZ2lzdHJ5XFxcIilcXG5cXG4gICAgICAgIHRydWUpKVxcblxcbiAgKGRlZnVuIHdyaXRlLXJlZ2lzdHJ5OnN0cmluZ1xcbiAgICAgICggbnMtbmFtZTpzdHJpbmdcXG4gICAgICAgIGd1YXJkOmd1YXJkXFxuICAgICAgICBhY3RpdmU6Ym9vbFxcbiAgICAgICAgKVxcbiAgICBcXFwiIFdyaXRlIGVudHJ5IHdpdGggR1VBUkQgYW5kIEFDVElWRSBpbnRvIHJlZ2lzdHJ5IGZvciBOQU1FLiBcXFxcXFxuICAgIFxcXFwgR3VhcmRlZCBieSBvcGVyYXRlIGtleXNldC4gXFxcIlxcblxcbiAgICAod2l0aC1jYXBhYmlsaXR5IChPUEVSQVRFKVxcblxcbiAgICAgICh2YWxpZGF0ZS1uYW1lIG5zLW5hbWUpXFxuXFxuICAgICAgKHdyaXRlIHJlZ2lzdHJ5IG5zLW5hbWVcXG4gICAgICAgIHsgJ2FkbWluLWd1YXJkOiBndWFyZFxcbiAgICAgICAgLCAnYWN0aXZlOiBhY3RpdmUgfSlcXG5cXG4gICAgICBcXFwiUmVnaXN0ZXIgZW50cnkgd3JpdHRlblxcXCIpKVxcblxcbiAgKGRlZnVuIHF1ZXJ5Om9iamVjdHtyZWctZW50cnl9XFxuICAgICAgKCBucy1uYW1lOnN0cmluZyApXFxuICAgIChyZWFkIHJlZ2lzdHJ5IG5zLW5hbWUpKVxcblxcbiAgKVxcblxcbihjcmVhdGUtdGFibGUgcmVnaXN0cnkpXFxuXFxuKHdyaXRlLXJlZ2lzdHJ5IFxcXCJrYWRlbmFcXFwiXFxuICAoa2V5c2V0LXJlZi1ndWFyZCAnbnMtb3BlcmF0ZS1rZXlzZXQpIHRydWUpXFxuKHdyaXRlLXJlZ2lzdHJ5IFxcXCJ1c2VyXFxcIiBHVUFSRF9GQUlMVVJFIHRydWUpXFxuKHdyaXRlLXJlZ2lzdHJ5IFxcXCJmcmVlXFxcIiBHVUFSRF9GQUlMVVJFIHRydWUpXFxuXFxuKGRlZmluZS1uYW1lc3BhY2UgXFxcImthZGVuYVxcXCJcXG4gIChrZXlzZXQtcmVmLWd1YXJkICducy1vcGVyYXRlLWtleXNldClcXG4gIChrZXlzZXQtcmVmLWd1YXJkICducy1vcGVyYXRlLWtleXNldCkpXFxuXFxuKGRlZmluZS1uYW1lc3BhY2UgXFxcInVzZXJcXFwiIEdVQVJEX1NVQ0NFU1MgR1VBUkRfRkFJTFVSRSlcXG4oZGVmaW5lLW5hbWVzcGFjZSBcXFwiZnJlZVxcXCIgR1VBUkRfU1VDQ0VTUyBHVUFSRF9GQUlMVVJFKVxcblwifX0sXCJzaWduZXJzXCI6W3tcInB1YktleVwiOlwiMzY4ODIwZjgwYzMyNGJiYzdjMmIwNjEwNjg4YTdkYTQzZTM5ZjkxZDExODczMjY3MWNkOWM3NTAwZmY0M2NjYVwifV0sXCJtZXRhXCI6e1wiY3JlYXRpb25UaW1lXCI6MCxcInR0bFwiOjE3MjgwMCxcImdhc0xpbWl0XCI6MCxcImNoYWluSWRcIjpcIlwiLFwiZ2FzUHJpY2VcIjowLFwic2VuZGVyXCI6XCJcIn0sXCJub25jZVwiOlwiXFxcImxvYWQtbnMtcHJvZG5ldC1zZW5kZXIwMFxcXCJcIn0ifQ
  - eyJnYXMiOjAsInJlc3VsdCI6eyJzdGF0dXMiOiJzdWNjZXNzIiwiZGF0YSI6Ik5hbWVzcGFjZSBkZWZpbmVkOiBmcmVlIn0sInJlcUtleSI6Ii1SZGc1YnZQdW45b05WLUNpdjljai1BN2FYX0VYTHM5UkFTUnRUT29nY1UiLCJsb2dzIjoiRHdwc2xkQVhleE1qSXpjYWt3M29XQWxKMkltX1pXVDNqZDkwZ0J0VExXayIsIm1ldGFEYXRhIjpudWxsLCJjb250aW51YXRpb24iOm51bGwsInR4SWQiOjF9
- - eyJoYXNoIjoiU3ZuMEs5QzF0UzdwMTlzWEJtS0xWS2IwS2RNOXFraEl3OFlRTS1KdGxVYyIsInNpZ3MiOltdLCJjbWQiOiJ7XCJuZXR3b3JrSWRcIjpudWxsLFwicGF5bG9hZFwiOntcImV4ZWNcIjp7XCJkYXRhXCI6e1wiZW1pbHlcIjpbXCI3N2E0OTQ3YzQzY2RiNGI0ZmRlZjAwYjEwZGM4NDQ2YzBlNTkxYjczMzY2ODYxMDQ4YzE5NWJiNmE1OWJmNWI5XCJdfSxcImNvZGVcIjpcIlxcbihkZWZpbmUta2V5c2V0IFxcXCJlbWlseVxcXCIgKHJlYWQta2V5c2V0IFxcXCJlbWlseVxcXCIpKVxcbihjb2luLmNyZWF0ZS1hbGxvY2F0aW9uLWFjY291bnQgXFxcImVtaWx5XFxcIiAodGltZSBcXFwiMjAyMC0xMC0zMVQwMDowMDowMFpcXFwiKSBcXFwiZW1pbHlcXFwiIDIwMDAwMC4wKVwifX0sXCJzaWduZXJzXCI6W10sXCJtZXRhXCI6e1wiY3JlYXRpb25UaW1lXCI6MCxcInR0bFwiOjE3MjgwMCxcImdhc0xpbWl0XCI6MCxcImNoYWluSWRcIjpcIlwiLFwiZ2FzUHJpY2VcIjowLFwic2VuZGVyXCI6XCJcIn0sXCJub25jZVwiOlwiXFxcInByb2RuZXQtZ3JhbnRzXFxcIlwifSJ9
  - eyJnYXMiOjAsInJlc3VsdCI6eyJzdGF0dXMiOiJzdWNjZXNzIiwiZGF0YSI6IldyaXRlIHN1Y2NlZWRlZCJ9LCJyZXFLZXkiOiJTdm4wSzlDMXRTN3AxOXNYQm1LTFZLYjBLZE05cWtoSXc4WVFNLUp0bFVjIiwibG9ncyI6InQzX3dfdWtRc2Fzc3V2M243SThqdV9aR0xsQU1TTzk0MXVha1ZjeU8wNHMiLCJtZXRhRGF0YSI6bnVsbCwiY29udGludWF0aW9uIjpudWxsLCJ0eElkIjoyfQ
- - eyJoYXNoIjoieU9Bd2JVZlBZMXVCMlhrWGZyUnpsV1NzX29iZzZBV3hrR2FUYTc1a0VhdyIsInNpZ3MiOltdLCJjbWQiOiJ7XCJuZXR3b3JrSWRcIjpudWxsLFwicGF5bG9hZFwiOntcImV4ZWNcIjp7XCJkYXRhXCI6e1widG9ueVwiOltcIjcyNzM2MGMzMmUwODgxMmZmNWYwNGNiMWU3NWNkOWMwYmE2YjcyNjg0MDg4NjE2MDEwYWY0MTFkMGYxNGM1NGFcIl0sXCJlbWlseVwiOltcIjc3YTQ5NDdjNDNjZGI0YjRmZGVmMDBiMTBkYzg0NDZjMGU1OTFiNzMzNjY4NjEwNDhjMTk1YmI2YTU5YmY1YjlcIl0sXCJjb2xpblwiOltcImRiNzc2NzkzYmUwZmNmOGU3NmM3NWJkYjM1YTM2ZTY3ZjI5ODExMWRjNjE0NWM2NjY5M2IwMTMzMTkyZTI2MTZcIl0sXCJtb25pY2FcIjpbXCI3YWM3MTFjNWQ0NTk4MzA3ZmE2YWIzZTUwNTFlNmViN2RiYzdjOGMwNWVkZTViZWE3NWJlODc2ZTM4NzhkZmY4XCJdLFwibWlndWVsXCI6W1wiOTVkMDQyOWU0NWJhMDc1YWE2ZmRhODEyNjNkZjU4MzlkNTIwMjBjNDIxMjIyMjBkOWQ4NDFhNGNlMmY0MjJkY1wiXSxcInRheWxvclwiOltcIjdlYjcyMmQxM2EzZjJkNzg1NDgyYWVmMmJjOGJiNDlkOTY4NDliZDVkMzk1YTQwY2VkOGIwNWJmZTA2OGY0NGJcIl0sXCJtYXJrXCI6W1wiZTlkZDMzNzYxMzk3OTkxYTMxN2U3Y2QyNGU4MmRmNGQxZTEyYTMwMDYyYTI0MmM5ZTFjNWY2YTE5MzA1MTZkY1wiXSxcImFuYWdoYVwiOltcIjBkOGRjYWUzYjZmZDgwNWM1NTJlYTNiY2E5M2Q5Nzg3NjI0MzExZjc4NGY5OTJiZTNhMWVjZDQ0NjEwMmNjZDlcIl0sXCJhbmRyZXdcIjpbXCI3ZDdhMDliNzQxZTY3MjYyMTk3MDlmN2FiOTc5MGM2OWY4Y2QxMmIxMDI2ZDAwOTBhYzdjNTJlOWFhMGFiNzRiXCJdLFwiYW5hc3Rhc2lhXCI6W1wiMWVjNzRkZTcyNGIzYzcwMDczN2Q5OTkwNmFiYTQ4ZGJlZDZiOTAxNDBiYmM2NDEzM2M1NTg1NzFiMTg4ODNjM1wiXSxcImNyb2VzdXNcIjpbXCIyOTkzZjc5NWQxMzNmYTVkMGZkODc3YTY0MWNhYmM4YjI4Y2QzNjE0N2Y2NjY5ODhjYWNiYWE0Mzc5ZDFmZjkzXCJdLFwiZW1tYW51ZWxcIjpbXCJhNzg0YTUyOTg0MjI5ZWQyOGI3MDM4MTBjZDQ1MjU2NTc1OWQ4ODQyMDg3NWE1ZGIyNTdmYThlODA2YWM5ZGU1XCJdLFwiYmVuXCI6W1wiZDhlNTEyYjJkMDhhZmFmMjBjNTlhYWYyYTBkYjU3MjYyYzMxOTY1ZTQzMTBlMzMwYzFhZDgwNzVlY2FhMzY0N1wiXSxcImRvdWdcIjpbXCJkZWE2NDcwMDkyOTVkYzAxNWJhNmU2MzU5Yjg1YmFmZTA5ZDJjZTkzNWEwM2MzYjNmNzc1NDQyZDUzOTAyNVwiXSxcInZpdlwiOltcIjIzZDRiNGU4Y2IzMDNmZWViY2FmYTkzMDczMWQ0Yjg5MTM2OTViZDBkOTJjM2I1MzU5NTA2NTE5Y2ViZjE1NmFcIl0sXCJqZWZmXCI6W1wiOTg5M2FiZDNjODU4ODlkZmEzNTFjNWVlODYxN2U1YzE5MGZhM2E4OGNlNDVhNmFlY2MxNDUwYzZhNjg3MWU3ZVwiXSxcImZyYW5jZXNjb1wiOltcIjRiMGYyOWI5ZTBhOTk2NTg3ZTViNTUyNDczMWM5MWVjZjAyZWNhYThiN2U3MGU1ZjhmMTg4MWM1ZjBjMThmYzFcIl0sXCJzdHVhcnRcIjpbXCIwZTMxODBkZDRhZTlkNDM4NWEyMWQ3Y2E5NWNjNzIyOWIwZjZiMmNhOWUwZjQ4NjFkZDRlZWQ2ODUwMDRkZjY2XCJdLFwibGVhaFwiOltcIjIxNjY0MzEzN2ZiNWY2YTEyN2RiYjEwNjM3MDJjYmI3YzFmMTUzNDUxYTFhZDRhZmE5ZjkyZTc3NDQ3Y2MzMzBcIl0sXCJ3aWxsXCI6W1wiODMwYWQ3NzM1MTBhYjBmNWEwZDQ1NjRiN2IzMWQyYjBiMWJhN2ZkNWExMTZmNjY1OThhMWM2M2NjMjE0MmZkNVwiXSxcImxpbmRhXCI6W1wiZTEwMDJkYjA4OTQyOTliYjM3MjM4YmFiMDIxNzAxODBjMGFlNTZmNTkzMjE2OWJiMmZjZjc1NjA1OWZlYTE3NVwiXSxcImhlZWt5dW5cIjpbXCIwOTA0ZjU5NzJiMmJjMjYzY2EzYzU2YjZiOWZiODg0NjRhY2JhYzQ5MGMyMjE5N2MwMGE3NzhjOWI3MGMyMGNkXCJdLFwicmViZWNjYVwiOltcIjJmZWE0M2Y3YzgwYmE3NWQyMTczYzdkODMyMjg3MDliMTQ0YzU3YzVlODYzNDdjODIwNzMxZTAyNWJkN2YyYmNcIl19LFwiY29kZVwiOlwiKGNvaW4uY29pbmJhc2UgXFxcImNyb2VzdXNcXFwiIChyZWFkLWtleXNldCBcXFwiY3JvZXN1c1xcXCIpIDkwMDAwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwiYW5hZ2hhXFxcIiAocmVhZC1rZXlzZXQgXFxcImFuYWdoYVxcXCIpIDUwMC4wKVxcbihjb2luLmNvaW5iYXNlIFxcXCJhbmFzdGFzaWFcXFwiIChyZWFkLWtleXNldCBcXFwiYW5hc3Rhc2lhXFxcIikgNTAwLjApXFxuKGNvaW4uY29pbmJhc2UgXFxcImFuZHJld1xcXCIgKHJlYWQta2V5c2V0IFxcXCJhbmRyZXdcXFwiKSA1MDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwiYmVuXFxcIiAocmVhZC1rZXlzZXQgXFxcImJlblxcXCIpIDUwMC4wKVxcbihjb2luLmNvaW5iYXNlIFxcXCJjb2xpblxcXCIgKHJlYWQta2V5c2V0IFxcXCJjb2xpblxcXCIpIDUwMC4wKVxcbihjb2luLmNvaW5iYXNlIFxcXCJkb3VnXFxcIiAocmVhZC1rZXlzZXQgXFxcImRvdWdcXFwiKSA1MDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwiZW1pbHlcXFwiIChrZXlzZXQtcmVmLWd1YXJkIFxcXCJlbWlseVxcXCIpIDUwMC4wKVxcbihjb2luLmNvaW5iYXNlIFxcXCJlbW1hbnVlbFxcXCIgKHJlYWQta2V5c2V0IFxcXCJlbW1hbnVlbFxcXCIpIDUwMC4wKVxcbihjb2luLmNvaW5iYXNlIFxcXCJmcmFuY2VzY29cXFwiIChyZWFkLWtleXNldCBcXFwiZnJhbmNlc2NvXFxcIikgNTAwLjApXFxuKGNvaW4uY29pbmJhc2UgXFxcImhlZWt5dW5cXFwiIChyZWFkLWtleXNldCBcXFwiaGVla3l1blxcXCIpIDUwMC4wKVxcbihjb2luLmNvaW5iYXNlIFxcXCJqZWZmXFxcIiAocmVhZC1rZXlzZXQgXFxcImplZmZcXFwiKSA1MDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwibGVhaFxcXCIgKHJlYWQta2V5c2V0IFxcXCJsZWFoXFxcIikgNTAwLjApXFxuKGNvaW4uY29pbmJhc2UgXFxcImxpbmRhXFxcIiAocmVhZC1rZXlzZXQgXFxcImxpbmRhXFxcIikgNTAwLjApXFxuKGNvaW4uY29pbmJhc2UgXFxcIm1hcmtcXFwiIChyZWFkLWtleXNldCBcXFwibWFya1xcXCIpIDUwMC4wKVxcbihjb2luLmNvaW5iYXNlIFxcXCJtaWd1ZWxcXFwiIChyZWFkLWtleXNldCBcXFwibWlndWVsXFxcIikgNTAwLjApXFxuKGNvaW4uY29pbmJhc2UgXFxcIm1vbmljYVxcXCIgKHJlYWQta2V5c2V0IFxcXCJtb25pY2FcXFwiKSA1MDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwicmViZWNjYVxcXCIgKHJlYWQta2V5c2V0IFxcXCJyZWJlY2NhXFxcIikgNTAwLjApXFxuKGNvaW4uY29pbmJhc2UgXFxcInN0dWFydFxcXCIgKHJlYWQta2V5c2V0IFxcXCJzdHVhcnRcXFwiKSA1MDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwidGF5bG9yXFxcIiAocmVhZC1rZXlzZXQgXFxcInRheWxvclxcXCIpIDUwMC4wKVxcbihjb2luLmNvaW5iYXNlIFxcXCJ0b255XFxcIiAocmVhZC1rZXlzZXQgXFxcInRvbnlcXFwiKSA1MDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwidml2XFxcIiAocmVhZC1rZXlzZXQgXFxcInZpdlxcXCIpIDUwMC4wKVxcbihjb2luLmNvaW5iYXNlIFxcXCJ3aWxsXFxcIiAocmVhZC1rZXlzZXQgXFxcIndpbGxcXFwiKSA1MDAuMClcIn19LFwic2lnbmVyc1wiOltdLFwibWV0YVwiOntcImNyZWF0aW9uVGltZVwiOjAsXCJ0dGxcIjoxNzI4MDAsXCJnYXNMaW1pdFwiOjAsXCJjaGFpbklkXCI6XCJcIixcImdhc1ByaWNlXCI6MCxcInNlbmRlclwiOlwiXCJ9LFwibm9uY2VcIjpcIlxcXCJwcm9kbmV0LWdyYW50c1xcXCJcIn0ifQ
  - eyJnYXMiOjAsInJlc3VsdCI6eyJzdGF0dXMiOiJzdWNjZXNzIiwiZGF0YSI6IldyaXRlIHN1Y2NlZWRlZCJ9LCJyZXFLZXkiOiJ5T0F3YlVmUFkxdUIyWGtYZnJSemxXU3Nfb2JnNkFXeGtHYVRhNzVrRWF3IiwibG9ncyI6IlFhdVlCcEszMVpIcUlSUnRjZ0dGQUxVZllSdDdfMF85T0NWSkdySUZ3aEkiLCJtZXRhRGF0YSI6bnVsbCwiY29udGludWF0aW9uIjpudWxsLCJ0eElkIjozfQ
minerData: eyJhY2NvdW50IjoiTm9NaW5lciIsInByZWRpY2F0ZSI6IjwiLCJwdWJsaWMta2V5cyI6W119
transactionsHash: Cm4shmLWvasJnUOF7M59ri4bg3T8_wgbfLI-criOLt8
outputsHash: 1ofd-QavJ1bakgfarYgsPDtXn1L4fUU10LOF5Ep3bDo
payloadHash: eerzhVWfS6NnSVRbxLJ1yaT0AbtOCZcngCTXi1qOXRA
coinbase: eyJnYXMiOjAsInJlc3VsdCI6eyJzdGF0dXMiOiJzdWNjZXNzIiwiZGF0YSI6Ik5PX0NPSU5CQVNFIn0sInJlcUtleSI6IkRsZFJ3Q2JsUTdMb3F5NndZSm5hb2RIbDMwZDNqM2VILXF0RnpmRXY0NmciLCJsb2dzIjpudWxsLCJtZXRhRGF0YSI6bnVsbCwiY29udGludWF0aW9uIjpudWxsLCJ0eElkIjpudWxsfQ

|]
