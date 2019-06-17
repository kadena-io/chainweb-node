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
- - eyJoYXNoIjoibVF4Mk5reVc1cVNPbVA2RlNMb21COVpETjJSUk9STkgyQWJxMXIwNXUzayIsInNpZ3MiOltdLCJjbWQiOiJ7XCJwYXlsb2FkXCI6e1wiZXhlY1wiOntcImRhdGFcIjpudWxsLFwiY29kZVwiOlwiKG1vZHVsZSBjb2luIEdPVkVSTkFOQ0VcXG5cXG4gIFxcXCInY29pbicgcmVwcmVzZW50cyB0aGUgS2FkZW5hIENvaW4gQ29udHJhY3QuIFRoaXMgY29udHJhY3QgcHJvdmlkZXMgYm90aCB0aGUgXFxcXFxcbiAgXFxcXGJ1eS9yZWRlZW0gZ2FzIHN1cHBvcnQgaW4gdGhlIGZvcm0gb2YgJ2Z1bmQtdHgnLCBhcyB3ZWxsIGFzIHRyYW5zZmVyLCAgICAgICBcXFxcXFxuICBcXFxcY3JlZGl0LCBkZWJpdCwgY29pbmJhc2UsIGFjY291bnQgY3JlYXRpb24gYW5kIHF1ZXJ5LCBhcyB3ZWxsIGFzIFNQViBidXJuICAgIFxcXFxcXG4gIFxcXFxjcmVhdGUuIFRvIGFjY2VzcyB0aGUgY29pbiBjb250cmFjdCwgeW91IG1heSB1c2UgaXRzIGZ1bGx5LXF1YWxpZmllZCBuYW1lLCAgXFxcXFxcbiAgXFxcXG9yIGlzc3VlIHRoZSAnKHVzZSBjb2luKScgY29tbWFuZCBpbiB0aGUgYm9keSBvZiBhIG1vZHVsZSBkZWNsYXJhdGlvbi5cXFwiXFxuXFxuXFxuICA7KHVzZSBjb2luLXNpZylcXG5cXG4gIDsgLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS1cXG4gIDsgU2NoZW1hcyBhbmQgVGFibGVzXFxuXFxuICAoZGVmc2NoZW1hIGNvaW4tc2NoZW1hXFxuICAgIGJhbGFuY2U6ZGVjaW1hbFxcbiAgICBndWFyZDpndWFyZFxcbiAgICApXFxuICAoZGVmdGFibGUgY29pbi10YWJsZTp7Y29pbi1zY2hlbWF9KVxcblxcbiAgKGRlZnNjaGVtYSBjcmVhdGVzLXNjaGVtYVxcbiAgICBleGlzdHM6Ym9vbFxcbiAgICApXFxuICAoZGVmdGFibGUgY3JlYXRlcy10YWJsZTp7Y3JlYXRlcy1zY2hlbWF9KVxcblxcbiAgOyAtLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLVxcbiAgOyBDYXBhYmlsaXRpZXNcXG5cXG4gIChkZWZjYXAgR09WRVJOQU5DRSAoKVxcbiAgICBcXFwidXBncmFkZSBkaXNhYmxlZFxcXCJcXG4gICAgZmFsc2UpXFxuXFxuICAoZGVmY2FwIFRSQU5TRkVSICgpXFxuICAgIFxcXCJBdXRvbm9tb3VzIGNhcGFiaWxpdHkgdG8gcHJvdGVjdCBkZWJpdCBhbmQgY3JlZGl0IGFjdGlvbnNcXFwiXFxuICAgIHRydWUpXFxuXFxuICAoZGVmY2FwIENPSU5CQVNFICgpXFxuICAgIFxcXCJNYWdpYyBjYXBhYmlsaXR5IHRvIHByb3RlY3QgbWluZXIgcmV3YXJkXFxcIlxcbiAgICB0cnVlKVxcblxcbiAgKGRlZmNhcCBGVU5EX1RYICgpXFxuICAgIFxcXCJNYWdpYyBjYXBhYmlsaXR5IHRvIGV4ZWN1dGUgZ2FzIHB1cmNoYXNlcyBhbmQgcmVkZW1wdGlvbnNcXFwiXFxuICAgIHRydWUpXFxuXFxuICAoZGVmY2FwIEJVUk5fQ1JFQVRFICgpXFxuICAgIFxcXCJNYWdpYyBjYXBhYmlsaXR5IHRvIHByb3RlY3QgY3Jvc3MtY2hhaW4gY29pbiB0cmFuc2ZlcnNcXFwiXFxuICAgIHRydWUpXFxuXFxuICAoZGVmY2FwIEFDQ09VTlRfR1VBUkQgKGFjY291bnQpXFxuICAgIFxcXCJMb29rdXAgYW5kIGVuZm9yY2UgZ3VhcmRzIGFzc29jaWF0ZWQgd2l0aCBhbiBhY2NvdW50XFxcIlxcbiAgICAod2l0aC1yZWFkIGNvaW4tdGFibGUgYWNjb3VudCB7IFxcXCJndWFyZFxcXCIgOj0gZyB9XFxuICAgICAgKGVuZm9yY2UtZ3VhcmQgZykpKVxcblxcbiAgKGRlZmNhcCBHT1ZFUk5BTkNFICgpXFxuICAgIChlbmZvcmNlIGZhbHNlIFxcXCJFbmZvcmNlIG5vbi11cGdyYWRlYWJpbGl0eSBleGNlcHQgaW4gdGhlIGNhc2Ugb2YgYSBoYXJkIGZvcmtcXFwiKSlcXG5cXG4gIDsgLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS1cXG4gIDsgQ29pbiBDb250cmFjdFxcblxcbiAgKGRlZnVuIGJ1eS1nYXM6c3RyaW5nIChzZW5kZXI6c3RyaW5nIHRvdGFsOmRlY2ltYWwpXFxuICAgIEBkb2MgXFxcIlRoaXMgZnVuY3Rpb24gZGVzY3JpYmVzIHRoZSBtYWluICdnYXMgYnV5JyBvcGVyYXRpb24uIEF0IHRoaXMgcG9pbnQgXFxcXFxcbiAgICBcXFxcTUlORVIgaGFzIGJlZW4gY2hvc2VuIGZyb20gdGhlIHBvb2wsIGFuZCB3aWxsIGJlIHZhbGlkYXRlZC4gVGhlIFNFTkRFUiAgIFxcXFxcXG4gICAgXFxcXG9mIHRoaXMgdHJhbnNhY3Rpb24gaGFzIHNwZWNpZmllZCBhIGdhcyBsaW1pdCBMSU1JVCAobWF4aW11bSBnYXMpIGZvciAgICBcXFxcXFxuICAgIFxcXFx0aGUgdHJhbnNhY3Rpb24sIGFuZCB0aGUgcHJpY2UgaXMgdGhlIHNwb3QgcHJpY2Ugb2YgZ2FzIGF0IHRoYXQgdGltZS4gICAgXFxcXFxcbiAgICBcXFxcVGhlIGdhcyBidXkgd2lsbCBiZSBleGVjdXRlZCBwcmlvciB0byBleGVjdXRpbmcgU0VOREVSJ3MgY29kZS5cXFwiXFxuXFxuICAgIEBtb2RlbCBbKHByb3BlcnR5ICg-IHRvdGFsIDAuMCkpXVxcblxcbiAgICAocmVxdWlyZS1jYXBhYmlsaXR5IChGVU5EX1RYKSlcXG4gICAgKHdpdGgtY2FwYWJpbGl0eSAoVFJBTlNGRVIpXFxuICAgICAgKGRlYml0IHNlbmRlciB0b3RhbCkpXFxuICAgIClcXG5cXG4gIChkZWZ1biByZWRlZW0tZ2FzOnN0cmluZyAobWluZXI6c3RyaW5nIG1pbmVyLWd1YXJkOmd1YXJkIHNlbmRlcjpzdHJpbmcgdG90YWw6ZGVjaW1hbClcXG4gICAgQGRvYyBcXFwiVGhpcyBmdW5jdGlvbiBkZXNjcmliZXMgdGhlIG1haW4gJ3JlZGVlbSBnYXMnIG9wZXJhdGlvbi4gQXQgdGhpcyAgICBcXFxcXFxuICAgIFxcXFxwb2ludCwgdGhlIFNFTkRFUidzIHRyYW5zYWN0aW9uIGhhcyBiZWVuIGV4ZWN1dGVkLCBhbmQgdGhlIGdhcyB0aGF0ICAgICAgXFxcXFxcbiAgICBcXFxcd2FzIGNoYXJnZWQgaGFzIGJlZW4gY2FsY3VsYXRlZC4gTUlORVIgd2lsbCBiZSBjcmVkaXRlZCB0aGUgZ2FzIGNvc3QsICAgIFxcXFxcXG4gICAgXFxcXGFuZCBTRU5ERVIgd2lsbCByZWNlaXZlIHRoZSByZW1haW5kZXIgdXAgdG8gdGhlIGxpbWl0XFxcIlxcblxcbiAgICBAbW9kZWwgWyhwcm9wZXJ0eSAoPiB0b3RhbCAwLjApKV1cXG5cXG4gICAgKHJlcXVpcmUtY2FwYWJpbGl0eSAoRlVORF9UWCkpXFxuICAgICh3aXRoLWNhcGFiaWxpdHkgKFRSQU5TRkVSKVxcbiAgICAgIChsZXQqICgoZmVlIChyZWFkLWRlY2ltYWwgXFxcImZlZVxcXCIpKVxcbiAgICAgICAgICAgICAocmVmdW5kICgtIHRvdGFsIGZlZSkpKVxcbiAgICAgICAgKGVuZm9yY2UgKD49IHJlZnVuZCAwLjApIFxcXCJmZWUgbXVzdCBiZSBsZXNzIHRoYW4gb3IgZXF1YWwgdG8gdG90YWxcXFwiKVxcblxcblxcbiAgICAgICAgOyBkaXJlY3RseSB1cGRhdGUgaW5zdGVhZCBvZiBjcmVkaXRcXG4gICAgICAgIChpZiAoPiByZWZ1bmQgMC4wKVxcbiAgICAgICAgICAod2l0aC1yZWFkIGNvaW4tdGFibGUgc2VuZGVyXFxuICAgICAgICAgICAgeyBcXFwiYmFsYW5jZVxcXCIgOj0gYmFsYW5jZSB9XFxuICAgICAgICAgICAgKHVwZGF0ZSBjb2luLXRhYmxlIHNlbmRlclxcbiAgICAgICAgICAgICAgeyBcXFwiYmFsYW5jZVxcXCI6ICgrIGJhbGFuY2UgcmVmdW5kKSB9KVxcbiAgICAgICAgICAgIClcXG4gICAgICAgICAgXFxcIm5vb3BcXFwiKVxcbiAgICAgICAgKGNyZWRpdCBtaW5lciBtaW5lci1ndWFyZCBmZWUpXFxuICAgICAgICApKVxcbiAgICApXFxuXFxuICAoZGVmdW4gY3JlYXRlLWFjY291bnQ6c3RyaW5nIChhY2NvdW50OnN0cmluZyBndWFyZDpndWFyZClcXG4gICAgQGRvYyBcXFwiQ3JlYXRlIGFuIGFjY291bnQgZm9yIEFDQ09VTlQsIHdpdGggQUNDT1VOVCBhcyBhIGZ1bmN0aW9uIG9mIEdVQVJEXFxcIlxcbiAgICAoaW5zZXJ0IGNvaW4tdGFibGUgYWNjb3VudFxcbiAgICAgIHsgXFxcImJhbGFuY2VcXFwiIDogMC4wXFxuICAgICAgLCBcXFwiZ3VhcmRcXFwiICAgOiBndWFyZFxcbiAgICAgIH0pXFxuICAgIClcXG5cXG4gIChkZWZ1biBhY2NvdW50LWJhbGFuY2U6ZGVjaW1hbCAoYWNjb3VudDpzdHJpbmcpXFxuICAgIEBkb2MgXFxcIlF1ZXJ5IGFjY291bnQgYmFsYW5jZSBmb3IgQUNDT1VOVFxcXCJcXG4gICAgKHdpdGgtcmVhZCBjb2luLXRhYmxlIGFjY291bnRcXG4gICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6PSBiYWxhbmNlIH1cXG4gICAgICBiYWxhbmNlXFxuICAgICAgKVxcbiAgICApXFxuXFxuICAoZGVmdW4gdHJhbnNmZXI6c3RyaW5nIChzZW5kZXI6c3RyaW5nIHJlY2VpdmVyOnN0cmluZyByZWNlaXZlci1ndWFyZDpndWFyZCBhbW91bnQ6ZGVjaW1hbClcXG4gICAgQGRvYyBcXFwiVHJhbnNmZXIgYmV0d2VlbiBhY2NvdW50cyBTRU5ERVIgYW5kIFJFQ0VJVkVSIG9uIHRoZSBzYW1lIGNoYWluLiAgICBcXFxcXFxuICAgIFxcXFxUaGlzIGZhaWxzIGlmIGJvdGggYWNjb3VudHMgZG8gbm90IGV4aXN0LiBDcmVhdGUtb24tdHJhbnNmZXIgY2FuIGJlICAgICAgXFxcXFxcbiAgICBcXFxcaGFuZGxlZCBieSBzZW5kaW5nIGluIGEgY3JlYXRlIGNvbW1hbmQgaW4gdGhlIHNhbWUgdHguXFxcIlxcblxcbiAgICBAbW9kZWwgWyhwcm9wZXJ0eSAoPiBhbW91bnQgMC4wKSldXFxuXFxuICAgICh3aXRoLWNhcGFiaWxpdHkgKFRSQU5TRkVSKVxcbiAgICAgIChkZWJpdCBzZW5kZXIgYW1vdW50KVxcbiAgICAgIChjcmVkaXQgcmVjZWl2ZXIgcmVjZWl2ZXItZ3VhcmQgYW1vdW50KSlcXG4gICAgKVxcblxcbiAgKGRlZnVuIGNvaW5iYXNlOnN0cmluZyAoYWRkcmVzczpzdHJpbmcgYWRkcmVzcy1ndWFyZDpndWFyZCBhbW91bnQ6ZGVjaW1hbClcXG4gICAgQGRvYyBcXFwiTWludCBzb21lIG51bWJlciBvZiB0b2tlbnMgYW5kIGFsbG9jYXRlIHRoZW0gdG8gc29tZSBhZGRyZXNzXFxcIlxcbiAgICAocmVxdWlyZS1jYXBhYmlsaXR5IChDT0lOQkFTRSkpXFxuICAgICh3aXRoLWNhcGFiaWxpdHkgKFRSQU5TRkVSKVxcbiAgICAgKGNyZWRpdCBhZGRyZXNzIGFkZHJlc3MtZ3VhcmQgYW1vdW50KSkpXFxuXFxuICAoZGVmcGFjdCBmdW5kLXR4IChzZW5kZXIgbWluZXIgbWluZXItZ3VhcmQgdG90YWwpXFxuICAgIEBkb2MgXFxcIidmdW5kLXR4JyBpcyBhIHNwZWNpYWwgcGFjdCB0byBmdW5kIGEgdHJhbnNhY3Rpb24gaW4gdHdvIHN0ZXBzLCAgICAgXFxcXFxcbiAgICBcXFxcd2l0aCB0aGUgYWN0dWFsIHRyYW5zYWN0aW9uIHRyYW5zcGlyaW5nIGluIHRoZSBtaWRkbGU6ICAgICAgICAgICAgICAgICAgIFxcXFxcXG4gICAgXFxcXCAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBcXFxcXFxuICAgIFxcXFwgIDEpIEEgYnV5aW5nIHBoYXNlLCBkZWJpdGluZyB0aGUgc2VuZGVyIGZvciB0b3RhbCBnYXMgYW5kIGZlZSwgeWllbGRpbmcgXFxcXFxcbiAgICBcXFxcICAgICBUWF9NQVhfQ0hBUkdFLiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFxcXFxcXG4gICAgXFxcXCAgMikgQSBzZXR0bGVtZW50IHBoYXNlLCByZXN1bWluZyBUWF9NQVhfQ0hBUkdFLCBhbmQgYWxsb2NhdGluZyB0byB0aGUgICBcXFxcXFxuICAgIFxcXFwgICAgIGNvaW5iYXNlIGFjY291bnQgZm9yIHVzZWQgZ2FzIGFuZCBmZWUsIGFuZCBzZW5kZXIgYWNjb3VudCBmb3IgYmFsLSAgXFxcXFxcbiAgICBcXFxcICAgICBhbmNlICh1bnVzZWQgZ2FzLCBpZiBhbnkpLlxcXCJcXG5cXG4gICAgKHN0ZXAgKGJ1eS1nYXMgc2VuZGVyIHRvdGFsKSlcXG4gICAgKHN0ZXAgKHJlZGVlbS1nYXMgbWluZXIgbWluZXItZ3VhcmQgc2VuZGVyIHRvdGFsKSlcXG4gICAgKVxcblxcbiAgKGRlZnVuIGRlYml0OnN0cmluZyAoYWNjb3VudDpzdHJpbmcgYW1vdW50OmRlY2ltYWwpXFxuICAgIEBkb2MgXFxcIkRlYml0IEFNT1VOVCBmcm9tIEFDQ09VTlQgYmFsYW5jZSByZWNvcmRpbmcgREFURSBhbmQgREFUQVxcXCJcXG5cXG4gICAgQG1vZGVsIFsocHJvcGVydHkgKD4gYW1vdW50IDAuMCkpXVxcblxcbiAgICAocmVxdWlyZS1jYXBhYmlsaXR5IChUUkFOU0ZFUikpXFxuICAgICh3aXRoLWNhcGFiaWxpdHkgKEFDQ09VTlRfR1VBUkQgYWNjb3VudClcXG4gICAgICAod2l0aC1yZWFkIGNvaW4tdGFibGUgYWNjb3VudFxcbiAgICAgICAgeyBcXFwiYmFsYW5jZVxcXCIgOj0gYmFsYW5jZSB9XFxuXFxuICAgICAgICAoZW5mb3JjZSAoPD0gYW1vdW50IGJhbGFuY2UpIFxcXCJJbnN1ZmZpY2llbnQgZnVuZHNcXFwiKVxcbiAgICAgICAgKHVwZGF0ZSBjb2luLXRhYmxlIGFjY291bnRcXG4gICAgICAgICAgeyBcXFwiYmFsYW5jZVxcXCIgOiAoLSBiYWxhbmNlIGFtb3VudCkgfVxcbiAgICAgICAgICApKSlcXG4gICAgKVxcblxcblxcbiAgKGRlZnVuIGNyZWRpdDpzdHJpbmcgKGFjY291bnQ6c3RyaW5nIGd1YXJkOmd1YXJkIGFtb3VudDpkZWNpbWFsKVxcbiAgICBAZG9jIFxcXCJDcmVkaXQgQU1PVU5UIHRvIEFDQ09VTlQgYmFsYW5jZSByZWNvcmRpbmcgREFURSBhbmQgREFUQVxcXCJcXG5cXG4gICAgQG1vZGVsIFsocHJvcGVydHkgKD4gYW1vdW50IDAuMCkpXVxcblxcbiAgICAocmVxdWlyZS1jYXBhYmlsaXR5IChUUkFOU0ZFUikpXFxuICAgICh3aXRoLWRlZmF1bHQtcmVhZCBjb2luLXRhYmxlIGFjY291bnRcXG4gICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6IDAuMCwgXFxcImd1YXJkXFxcIiA6IGd1YXJkIH1cXG4gICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6PSBiYWxhbmNlLCBcXFwiZ3VhcmRcXFwiIDo9IHJldGcgfVxcbiAgICAgIDsgd2UgZG9uJ3Qgd2FudCB0byBvdmVyd3JpdGUgYW4gZXhpc3RpbmcgZ3VhcmQgd2l0aCB0aGUgdXNlci1zdXBwbGllZCBvbmVcXG4gICAgICAoZW5mb3JjZSAoPSByZXRnIGd1YXJkKVxcbiAgICAgICAgXFxcImFjY291bnQgZ3VhcmRzIGRvIG5vdCBtYXRjaFxcXCIpXFxuXFxuICAgICAgKHdyaXRlIGNvaW4tdGFibGUgYWNjb3VudFxcbiAgICAgICAgeyBcXFwiYmFsYW5jZVxcXCIgOiAoKyBiYWxhbmNlIGFtb3VudClcXG4gICAgICAgICwgXFxcImd1YXJkXFxcIiAgIDogcmV0Z1xcbiAgICAgICAgfSlcXG4gICAgICApKVxcblxcbiAgKGRlZnBhY3QgY3Jvc3MtY2hhaW4tdHJhbnNmZXIgKGRlbGV0ZS1hY2NvdW50IGNyZWF0ZS1jaGFpbi1pZCBjcmVhdGUtYWNjb3VudCBjcmVhdGUtYWNjb3VudC1ndWFyZCBxdWFudGl0eSlcXG5cXG4gICAgQGRvYyBcXFwiU3RlcCAxOiBCdXJuIFFVQU5USVRZLW1hbnkgY29pbnMgZm9yIERFTEVURS1BQ0NPVU5UIG9uIHRoZSBjdXJyZW50IGNoYWluLCBhbmQgXFxcXFxcbiAgICAgICAgIFxcXFxwcm9kdWNlIGFuIFNQViByZWNlaXB0IHdoaWNoIG1heSBiZSBtYW51YWxseSByZWRlZW1lZCBmb3IgYW4gU1BWICAgICAgXFxcXFxcbiAgICAgICAgIFxcXFxwcm9vZi4gT25jZSBhIHByb29mIGlzIG9idGFpbmVkLCB0aGUgdXNlciBtYXkgY2FsbCAnY3JlYXRlLWNvaW4nIGFuZCAgXFxcXFxcbiAgICAgICAgIFxcXFxjb25zdW1lIHRoZSBwcm9vZiBvbiBDUkVBVEUtQ0hBSU4tSUQsIGNyZWRpdGluZyBDUkVBVEUtQUNDT1VOVCAgICAgICAgXFxcXFxcbiAgICAgICAgIFxcXFxRVUFOVElUWS1tYW55IGNvaW5zLiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXFxcXFxcbiAgICAgICAgIFxcXFwgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXFxcXFxcbiAgICAgICAgIFxcXFxTdGVwIDI6IENvbnN1bWUgYW4gU1BWIHByb29mIGZvciBhIG51bWJlciBvZiBjb2lucywgYW5kIGNyZWRpdCB0aGUgICAgXFxcXFxcbiAgICAgICAgIFxcXFxhY2NvdW50IGFzc29jaWF0ZWQgd2l0aCB0aGUgcHJvb2YgdGhlIHF1YW50aWZ5IG9mIGNvaW5zIGJ1cm5lZCBvbiB0aGUgXFxcXFxcbiAgICAgICAgIFxcXFxzb3VyY2UgY2hhaW4gYnkgdGhlIGJ1cm4gYWNjb3VudC4gTm90ZTogbXVzdCBiZSBjYWxsZWQgb24gdGhlIGNvcnJlY3QgXFxcXFxcbiAgICAgICAgIFxcXFxjaGFpbiBpZCBhcyBzcGVjaWZpZWQgaW4gdGhlIHByb29mLlxcXCJcXG5cXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5ICg-IHF1YW50aXR5IDAuMCkpIF1cXG4gICAgKHN0ZXBcXG4gICAgICAod2l0aC1jYXBhYmlsaXR5IChCVVJOX0NSRUFURSlcXG4gICAgICAgIChsZXRcXG4gICAgICAgICAgKChyZXR2XFxuICAgICAgICAgICAgIChkZWxldGUtY29pbiBkZWxldGUtYWNjb3VudCBjcmVhdGUtY2hhaW4taWQgY3JlYXRlLWFjY291bnQgY3JlYXRlLWFjY291bnQtZ3VhcmQgcXVhbnRpdHkpXFxuICAgICAgICAgICAgICkpXFxuXFxuICAgICAgICAgICh5aWVsZCByZXR2KVxcbiAgICAgICAgICByZXR2KSkpXFxuXFxuICAgIChzdGVwXFxuICAgICAgKHJlc3VtZVxcbiAgICAgICAgeyBcXFwiY3JlYXRlLWNoYWluLWlkXFxcIjo9IGNyZWF0ZS1jaGFpbi1pZFxcbiAgICAgICAgLCBcXFwiY3JlYXRlLWFjY291bnRcXFwiIDo9IGNyZWF0ZS1hY2NvdW50XFxuICAgICAgICAsIFxcXCJjcmVhdGUtYWNjb3VudC1ndWFyZFxcXCIgOj0gY3JlYXRlLWFjY291bnQtZ3VhcmRcXG4gICAgICAgICwgXFxcInF1YW50aXR5XFxcIiA6PSBxdWFudGl0eVxcbiAgICAgICAgLCBcXFwiZGVsZXRlLXR4LWhhc2hcXFwiIDo9IGRlbGV0ZS10eC1oYXNoXFxuICAgICAgICAsIFxcXCJkZWxldGUtY2hhaW4taWRcXFwiIDo9IGRlbGV0ZS1jaGFpbi1pZFxcbiAgICAgICAgfVxcblxcbiAgICAgICAgKHdpdGgtY2FwYWJpbGl0eSAoQlVSTl9DUkVBVEUpXFxuICAgICAgICAgIChjcmVhdGUtY29pblxcbiAgICAgICAgICAgIGNyZWF0ZS1jaGFpbi1pZFxcbiAgICAgICAgICAgIGNyZWF0ZS1hY2NvdW50XFxuICAgICAgICAgICAgY3JlYXRlLWFjY291bnQtZ3VhcmRcXG4gICAgICAgICAgICBxdWFudGl0eVxcbiAgICAgICAgICAgIGRlbGV0ZS10eC1oYXNoXFxuICAgICAgICAgICAgZGVsZXRlLWNoYWluLWlkXFxuICAgICAgICAgICAgKSkpKVxcbiAgICApXFxuXFxuICAoZGVmdW4gZGVsZXRlLWNvaW4gKGRlbGV0ZS1hY2NvdW50IGNyZWF0ZS1jaGFpbi1pZCBjcmVhdGUtYWNjb3VudCBjcmVhdGUtYWNjb3VudC1ndWFyZCBxdWFudGl0eSlcXG4gICAgKHJlcXVpcmUtY2FwYWJpbGl0eSAoQlVSTl9DUkVBVEUpKVxcbiAgICAod2l0aC1jYXBhYmlsaXR5IChUUkFOU0ZFUilcXG4gICAgICAoZGViaXQgZGVsZXRlLWFjY291bnQgcXVhbnRpdHkpKVxcblxcbiAgICAgIHsgXFxcImNyZWF0ZS1jaGFpbi1pZFxcXCI6IGNyZWF0ZS1jaGFpbi1pZFxcbiAgICAgICwgXFxcImNyZWF0ZS1hY2NvdW50XFxcIjogY3JlYXRlLWFjY291bnRcXG4gICAgICAsIFxcXCJjcmVhdGUtYWNjb3VudC1ndWFyZFxcXCI6IGNyZWF0ZS1hY2NvdW50LWd1YXJkXFxuICAgICAgLCBcXFwicXVhbnRpdHlcXFwiOiBxdWFudGl0eVxcbiAgICAgICwgXFxcImRlbGV0ZS1ibG9jay1oZWlnaHRcXFwiOiAoYXQgJ2Jsb2NrLWhlaWdodCAoY2hhaW4tZGF0YSkpXFxuICAgICAgLCBcXFwiZGVsZXRlLWNoYWluLWlkXFxcIjogKGF0ICdjaGFpbi1pZCAoY2hhaW4tZGF0YSkpXFxuICAgICAgLCBcXFwiZGVsZXRlLWFjY291bnRcXFwiOiBkZWxldGUtYWNjb3VudFxcbiAgICAgICwgXFxcImRlbGV0ZS10eC1oYXNoXFxcIjogKHR4LWhhc2gpXFxuICAgICAgfSlcXG5cXG4gIChkZWZ1biBjcmVhdGUtY29pbiAoY3JlYXRlLWNoYWluLWlkIGNyZWF0ZS1hY2NvdW50IGNyZWF0ZS1hY2NvdW50LWd1YXJkIHF1YW50aXR5IGRlbGV0ZS10eC1oYXNoIGRlbGV0ZS1jaGFpbi1pZClcXG5cXG4gICAgKHJlcXVpcmUtY2FwYWJpbGl0eSAoQlVSTl9DUkVBVEUpKVxcblxcbiAgICAoZW5mb3JjZSAoPSBjcmVhdGUtY2hhaW4taWQgKGF0IFxcXCJjaGFpbi1pZFxcXCIgKGNoYWluLWRhdGEpKSlcXG4gICAgICBcXFwiZW5mb3JjZSBjb3JyZWN0IGNyZWF0ZSBjaGFpbiBJRFxcXCIpXFxuXFxuICAgIChsZXQgKChjcmVhdGUtaWQgKGZvcm1hdCBcXFwie306e31cXFwiIFtkZWxldGUtdHgtaGFzaCBkZWxldGUtY2hhaW4taWRdKSkpXFxuICAgICAgKHdpdGgtZGVmYXVsdC1yZWFkIGNyZWF0ZXMtdGFibGUgY3JlYXRlLWlkXFxuICAgICAgICB7IFxcXCJleGlzdHNcXFwiOiBmYWxzZSB9XFxuICAgICAgICB7IFxcXCJleGlzdHNcXFwiOj0gZXhpc3RzIH1cXG5cXG4gICAgICAgIChlbmZvcmNlIChub3QgZXhpc3RzKVxcbiAgICAgICAgICAoZm9ybWF0IFxcXCJlbmZvcmNlIHVuaXF1ZSB1c2FnZSBvZiB7fVxcXCIgW2NyZWF0ZS1pZF0pKVxcblxcbiAgICAgICAgKGluc2VydCBjcmVhdGVzLXRhYmxlIGNyZWF0ZS1pZCB7IFxcXCJleGlzdHNcXFwiOiB0cnVlIH0pXFxuXFxuICAgICAgICAod2l0aC1jYXBhYmlsaXR5IChUUkFOU0ZFUilcXG4gICAgICAgICAgKGNyZWRpdCBjcmVhdGUtYWNjb3VudCBjcmVhdGUtYWNjb3VudC1ndWFyZCBxdWFudGl0eSkpXFxuXFxuICAgICAgICApKSlcXG4pXFxuXFxuKGNyZWF0ZS10YWJsZSBjb2luLXRhYmxlKVxcbihjcmVhdGUtdGFibGUgY3JlYXRlcy10YWJsZSlcXG5cIn19LFwic2lnbmVyc1wiOltdLFwibWV0YVwiOntcImdhc0xpbWl0XCI6MCxcImNoYWluSWRcIjpcIlwiLFwiZ2FzUHJpY2VcIjowLFwic2VuZGVyXCI6XCJcIn0sXCJub25jZVwiOlwiXFxcImdlbmVzaXMtMDFcXFwiXCJ9In0
  - eyJnYXMiOjAsInJlc3VsdCI6eyJzdGF0dXMiOiJzdWNjZXNzIiwiZGF0YSI6IlRhYmxlQ3JlYXRlZCJ9LCJyZXFLZXkiOiJtUXgyTmt5VzVxU09tUDZGU0xvbUI5WkROMlJST1JOSDJBYnExcjA1dTNrIiwibG9ncyI6IkFqQTJIVGpWdzJFdWdPRWFyRFoyMjlFR05XRUR5NXNNMDFNVUxLWTJZV2siLCJtZXRhRGF0YSI6bnVsbCwiY29udGludWF0aW9uIjpudWxsLCJ0eElkIjowfQ
- - eyJoYXNoIjoiUEoyb1NsaEJBVUtnU1ViaFFxY2RFNVBRa1lCNDkzSlVwZnNLYU54cmhjayIsInNpZ3MiOltdLCJjbWQiOiJ7XCJwYXlsb2FkXCI6e1wiZXhlY1wiOntcImRhdGFcIjp7XCJzZW5kZXIwN1wiOltcIjRjMzFkYzllZTdmMjQxNzdmNzhiNmY1MTgwMTJhMjA4MzI2ZTJhZjFmMzdiYjBhMjQwNWI1MDU2ZDBjYWQ2MjhcIl0sXCJzZW5kZXIwMVwiOltcIjZiZTJmNDg1YTdhZjc1ZmVkYjRiN2YxNTNhOTAzZjdlNjAwMGNhNGFhNTAxMTc5YzkxYTI0NTBiNzc3YmQyYTdcIl0sXCJzZW5kZXIwNlwiOltcIjVmZmMxZjdmZWY3YTQ0NzM4NjI1NzYyZjc1YTQyMjk0NTQ5NTFlMDNmMmFmYzZmODEzMDljMGMxYmRmOWVlNmZcIl0sXCJzZW5kZXIwMFwiOltcIjM2ODgyMGY4MGMzMjRiYmM3YzJiMDYxMDY4OGE3ZGE0M2UzOWY5MWQxMTg3MzI2NzFjZDljNzUwMGZmNDNjY2FcIl0sXCJzZW5kZXIwNVwiOltcImYwOWQ4ZjYzOTRhZWE0MjVmZTY3ODNkODhjZDgxMzYzZDgwMTdmMTZhZmQzNzExYzU3NWJlMGY1Y2Q1YzliYjlcIl0sXCJzZW5kZXIwNFwiOltcIjJkNzBhYTRmNjk3YzNhM2I4ZGQ2ZDk3NzQ1YWMwNzRlZGNmZDBlYjY1YzM3Nzc0Y2RlMjUxMzU0ODNiZWE3MWVcIl0sXCJtdWx0aS0wMi0wMy0wNC1hbnlcIjp7XCJwcmVkXCI6XCJrZXlzLWFueVwiLFwia2V5c1wiOltcIjNhOWRkNTMyZDczZGFjZTE5NWRiYjY0ZDFkYmE2NTcyZmI3ODNkMGZkZDMyNDY4NWUzMmZiZGEyZjg5Zjk5YTZcIixcIjQzZjJhZGIxZGUxOTIwMDBjYjM3NzdiYWNjN2Y5ODNiNjYxNGZkOWMxNzE1Y2Q0NGNkNDg0YjZkM2EwZDM0YzhcIixcIjJkNzBhYTRmNjk3YzNhM2I4ZGQ2ZDk3NzQ1YWMwNzRlZGNmZDBlYjY1YzM3Nzc0Y2RlMjUxMzU0ODNiZWE3MWVcIl19LFwic2VuZGVyMDlcIjpbXCJjNTlkOTg0MGIwYjY2MDkwODM2NTQ2YjdlYjRhNzM2MDYyNTc1MjdlYzhjMmI0ODIzMDBmZDIyOTI2NGIwN2U2XCJdLFwic2VuZGVyMDNcIjpbXCI0M2YyYWRiMWRlMTkyMDAwY2IzNzc3YmFjYzdmOTgzYjY2MTRmZDljMTcxNWNkNDRjZDQ4NGI2ZDNhMGQzNGM4XCJdLFwibXVsdGktMDAtMDFcIjpbXCIzNjg4MjBmODBjMzI0YmJjN2MyYjA2MTA2ODhhN2RhNDNlMzlmOTFkMTE4NzMyNjcxY2Q5Yzc1MDBmZjQzY2NhXCIsXCI2YmUyZjQ4NWE3YWY3NWZlZGI0YjdmMTUzYTkwM2Y3ZTYwMDBjYTRhYTUwMTE3OWM5MWEyNDUwYjc3N2JkMmE3XCJdLFwic2VuZGVyMDhcIjpbXCI2M2IyZWJhNGVkNzBkNDYxMmQzZTdiYzkwZGIyZmJmNGM3NmY3YjA3NDM2M2U4NmQ3M2YwYmM2MTdmOGU4YjgxXCJdLFwic2VuZGVyMDJcIjpbXCIzYTlkZDUzMmQ3M2RhY2UxOTVkYmI2NGQxZGJhNjU3MmZiNzgzZDBmZGQzMjQ2ODVlMzJmYmRhMmY4OWY5OWE2XCJdfSxcImNvZGVcIjpcIihjb2luLmNvaW5iYXNlIFxcXCJzZW5kZXIwMFxcXCIgKHJlYWQta2V5c2V0IFxcXCJzZW5kZXIwMFxcXCIpIDEwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDFcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDFcXFwiKSAxMDEwLjApXFxuKGNvaW4uY29pbmJhc2UgXFxcInNlbmRlcjAyXFxcIiAocmVhZC1rZXlzZXQgXFxcInNlbmRlcjAyXFxcIikgMTAyMC4wKVxcbihjb2luLmNvaW5iYXNlIFxcXCJzZW5kZXIwM1xcXCIgKHJlYWQta2V5c2V0IFxcXCJzZW5kZXIwM1xcXCIpIDEwMzAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDRcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDRcXFwiKSAxMDQwLjApXFxuKGNvaW4uY29pbmJhc2UgXFxcInNlbmRlcjA1XFxcIiAocmVhZC1rZXlzZXQgXFxcInNlbmRlcjA1XFxcIikgMTA1MC4wKVxcbihjb2luLmNvaW5iYXNlIFxcXCJzZW5kZXIwNlxcXCIgKHJlYWQta2V5c2V0IFxcXCJzZW5kZXIwNlxcXCIpIDEwNjAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDdcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDdcXFwiKSAxMDcwLjApXFxuKGNvaW4uY29pbmJhc2UgXFxcInNlbmRlcjA4XFxcIiAocmVhZC1rZXlzZXQgXFxcInNlbmRlcjA4XFxcIikgMTA4MC4wKVxcbihjb2luLmNvaW5iYXNlIFxcXCJzZW5kZXIwOVxcXCIgKHJlYWQta2V5c2V0IFxcXCJzZW5kZXIwOVxcXCIpIDEwOTAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwibXVsdGktMDAtMDFcXFwiIChyZWFkLWtleXNldCBcXFwibXVsdGktMDAtMDFcXFwiKSAxMDAxLjApXFxuKGNvaW4uY29pbmJhc2UgXFxcIm11bHRpLTAyLTAzLTA0LWFueVxcXCIgKHJlYWQta2V5c2V0IFxcXCJtdWx0aS0wMi0wMy0wNC1hbnlcXFwiKSAxMjM0LjApXCJ9fSxcInNpZ25lcnNcIjpbXSxcIm1ldGFcIjp7XCJnYXNMaW1pdFwiOjAsXCJjaGFpbklkXCI6XCJcIixcImdhc1ByaWNlXCI6MCxcInNlbmRlclwiOlwiXCJ9LFwibm9uY2VcIjpcIlxcXCJ0ZXN0bmV0MDAtZ3JhbnRzXFxcIlwifSJ9
  - eyJnYXMiOjAsInJlc3VsdCI6eyJzdGF0dXMiOiJzdWNjZXNzIiwiZGF0YSI6IldyaXRlIHN1Y2NlZWRlZCJ9LCJyZXFLZXkiOiJQSjJvU2xoQkFVS2dTVWJoUXFjZEU1UFFrWUI0OTNKVXBmc0thTnhyaGNrIiwibG9ncyI6Ik0tY2tlN29DS1QzelIxMGRHUEFKR2doZ1dVTkJmS096emVJLUN6WDd1bEUiLCJtZXRhRGF0YSI6bnVsbCwiY29udGludWF0aW9uIjpudWxsLCJ0eElkIjoxfQ
minerData: eyJtIjoiTm9NaW5lciIsImtzIjpbXSwia3AiOiI8In0
transactionsHash: 7nODta92iNOC3WmvFimlZkQq29mRMQRW5ucXG0z3sIc
outputsHash: ubulAAQ8D3P_vnOafQJWJ7pUsW2xDoMkAAAn6Qqwpwo
payloadHash: AHC_X5pzhvFDcKWgbu2q69TeOj429nVLjq_I7g6DEp0
coinbase: eyJnYXMiOjAsInJlc3VsdCI6eyJzdGF0dXMiOiJzdWNjZXNzIiwiZGF0YSI6Ik5PX0NPSU5CQVNFIn0sInJlcUtleSI6IkRsZFJ3Q2JsUTdMb3F5NndZSm5hb2RIbDMwZDNqM2VILXF0RnpmRXY0NmciLCJsb2dzIjpudWxsLCJtZXRhRGF0YSI6bnVsbCwiY29udGludWF0aW9uIjpudWxsLCJ0eElkIjpudWxsfQ

|]
