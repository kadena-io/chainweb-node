{-# LANGUAGE QuasiQuotes #-}

-- This module is auto-generated. DO NOT EDIT IT MANUALLY.

module Chainweb.BlockHeader.Genesis.DevelopmentPayload ( payloadBlock ) where

import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (decodeThrow)

import NeatInterpolation (text)

import Chainweb.Payload (PayloadWithOutputs)
import Chainweb.Utils (fromJuste)

payloadBlock :: PayloadWithOutputs
payloadBlock = fromJuste $ decodeThrow $ encodeUtf8 [text|
transactions:
- - eyJoYXNoIjoiRGNmbXNhWEtwT1oyemg5QmV6WmtseVlaYmhvamFTNlNBa1lVekNUZXdXVSIsInNpZ3MiOltdLCJjbWQiOiJ7XCJuZXR3b3JrSWRcIjpudWxsLFwicGF5bG9hZFwiOntcImV4ZWNcIjp7XCJkYXRhXCI6bnVsbCxcImNvZGVcIjpcIihtb2R1bGUgY29pbiBHT1ZFUk5BTkNFXFxuXFxuICBAZG9jIFxcXCInY29pbicgcmVwcmVzZW50cyB0aGUgS2FkZW5hIENvaW4gQ29udHJhY3QuIFRoaXMgY29udHJhY3QgcHJvdmlkZXMgYm90aCB0aGUgXFxcXFxcbiAgXFxcXGJ1eS9yZWRlZW0gZ2FzIHN1cHBvcnQgaW4gdGhlIGZvcm0gb2YgJ2Z1bmQtdHgnLCBhcyB3ZWxsIGFzIHRyYW5zZmVyLCAgICAgICBcXFxcXFxuICBcXFxcY3JlZGl0LCBkZWJpdCwgY29pbmJhc2UsIGFjY291bnQgY3JlYXRpb24gYW5kIHF1ZXJ5LCBhcyB3ZWxsIGFzIFNQViBidXJuICAgIFxcXFxcXG4gIFxcXFxjcmVhdGUuIFRvIGFjY2VzcyB0aGUgY29pbiBjb250cmFjdCwgeW91IG1heSB1c2UgaXRzIGZ1bGx5LXF1YWxpZmllZCBuYW1lLCAgXFxcXFxcbiAgXFxcXG9yIGlzc3VlIHRoZSAnKHVzZSBjb2luKScgY29tbWFuZCBpbiB0aGUgYm9keSBvZiBhIG1vZHVsZSBkZWNsYXJhdGlvbi5cXFwiXFxuXFxuICBAbW9kZWxcXG4gICAgWyAoZGVmcHJvcGVydHkgY29uc2VydmVzLW1hc3NcXG4gICAgICAgICg9IChjb2x1bW4tZGVsdGEgY29pbi10YWJsZSAnYmFsYW5jZSkgMC4wKSlcXG5cXG4gICAgICAoZGVmcHJvcGVydHkgYWNjb3VudCAoYWNjb3VudDpzdHJpbmcpXFxuICAgICAgICAoYW5kXFxuICAgICAgICAgICg-PSAobGVuZ3RoIGFjY291bnQpIDMpXFxuICAgICAgICAgICg8PSAobGVuZ3RoIGFjY291bnQpIDI1NikpKVxcbiAgICBdXFxuXFxuICA7IC0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tXFxuICA7IFNjaGVtYXMgYW5kIFRhYmxlc1xcblxcbiAgKGRlZnNjaGVtYSBjb2luLXNjaGVtYVxcbiAgICBAZG9jIFxcXCJUaGUgY29pbiBjb250cmFjdCB0b2tlbiBzY2hlbWFcXFwiXFxuICAgIEBtb2RlbCBbIChpbnZhcmlhbnQgKD49IGJhbGFuY2UgMC4wKSkgXSA7IEZWIHByb2JsZW1cXG5cXG4gICAgYmFsYW5jZTpkZWNpbWFsXFxuICAgIGd1YXJkOmd1YXJkKVxcblxcbiAgKGRlZnRhYmxlIGNvaW4tdGFibGU6e2NvaW4tc2NoZW1hfSlcXG5cXG4gIChkZWZzY2hlbWEgdHJhbnNmZXItc2NoZW1hXFxuICAgIEBkb2MgXFxcIkNvbnZlbmllbmNlIHNjaGVtYSBmb3IgdHlwZWNoZWNraW5nIGNyb3NzLWNoYWluIHRyYW5zZmVyc1xcXCJcXG5cXG4gICAgY3JlYXRlLWFjY291bnQ6c3RyaW5nXFxuICAgIGNyZWF0ZS1hY2NvdW50LWd1YXJkOmd1YXJkXFxuICAgIHF1YW50aXR5OmRlY2ltYWwpXFxuXFxuICA7IC0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tXFxuICA7IENhcGFiaWxpdGllc1xcblxcbiAgKGRlZmNhcCBUUkFOU0ZFUiAoKVxcbiAgICBcXFwiQXV0b25vbW91cyBjYXBhYmlsaXR5IHRvIHByb3RlY3QgZGViaXQgYW5kIGNyZWRpdCBhY3Rpb25zXFxcIlxcbiAgICB0cnVlKVxcblxcbiAgKGRlZmNhcCBDT0lOQkFTRSAoKVxcbiAgICBcXFwiTWFnaWMgY2FwYWJpbGl0eSB0byBwcm90ZWN0IG1pbmVyIHJld2FyZFxcXCJcXG4gICAgdHJ1ZSlcXG5cXG4gIChkZWZjYXAgR0VORVNJUyAoKVxcbiAgICBcXFwiTWFnaWMgY2FwYWJpbGl0eSBjb25zdHJhaW5pbmcgZ2VuZXNpcyB0eHNcXFwiXFxuICAgIHRydWUpXFxuXFxuICAoZGVmY2FwIEZVTkRfVFggKClcXG4gICAgXFxcIk1hZ2ljIGNhcGFiaWxpdHkgdG8gZXhlY3V0ZSBnYXMgcHVyY2hhc2VzIGFuZCByZWRlbXB0aW9uc1xcXCJcXG4gICAgdHJ1ZSlcXG5cXG4gIChkZWZjYXAgQUNDT1VOVF9HVUFSRCAoYWNjb3VudClcXG4gICAgXFxcIkxvb2t1cCBhbmQgZW5mb3JjZSBndWFyZHMgYXNzb2NpYXRlZCB3aXRoIGFuIGFjY291bnRcXFwiXFxuICAgICh3aXRoLXJlYWQgY29pbi10YWJsZSBhY2NvdW50IHsgXFxcImd1YXJkXFxcIiA6PSBnIH1cXG4gICAgICAoZW5mb3JjZS1ndWFyZCBnKSkpXFxuXFxuICAoZGVmY2FwIEdPVkVSTkFOQ0UgKClcXG4gICAgKGVuZm9yY2UgZmFsc2UgXFxcIkVuZm9yY2Ugbm9uLXVwZ3JhZGVhYmlsaXR5IGV4Y2VwdCBpbiB0aGUgY2FzZSBvZiBhIGhhcmQgZm9ya1xcXCIpKVxcblxcblxcbiAgOyAtLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLVxcbiAgOyBDb25zdGFudHNcXG5cXG4gIChkZWZjb25zdCBDT0lOX0NIQVJTRVQgQ0hBUlNFVF9MQVRJTjFcXG4gICAgXFxcIlRoZSBkZWZhdWx0IGNvaW4gY29udHJhY3QgY2hhcmFjdGVyIHNldFxcXCIpXFxuXFxuICAoZGVmY29uc3QgTUlOSU1VTV9QUkVDSVNJT04gMTJcXG4gICAgXFxcIk1pbmltdW0gYWxsb3dlZCBwcmVjaXNpb24gZm9yIGNvaW4gdHJhbnNhY3Rpb25zXFxcIilcXG5cXG4gIChkZWZjb25zdCBNSU5JTVVNX0FDQ09VTlRfTEVOR1RIIDNcXG4gICAgXFxcIk1pbmltdW0gYWNjb3VudCBsZW5ndGggYWRtaXNzaWJsZSBmb3IgY29pbiBhY2NvdW50c1xcXCIpXFxuXFxuICAoZGVmY29uc3QgTUFYSU1VTV9BQ0NPVU5UX0xFTkdUSCAyNTZcXG4gICAgXFxcIk1heGltdW0gYWNjb3VudCBuYW1lIGxlbmd0aCBhZG1pc3NpYmxlIGZvciBjb2luIGFjY291bnRzXFxcIilcXG5cXG4gIDsgLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS1cXG4gIDsgVXRpbGl0aWVzXFxuXFxuICAoZGVmdW4gZW5mb3JjZS11bml0IChhbW91bnQ6ZGVjaW1hbClcXG4gICAgQGRvYyBcXFwiRW5mb3JjZSBtaW5pbXVtIHByZWNpc2lvbiBhbGxvd2VkIGZvciBjb2luIHRyYW5zYWN0aW9uc1xcXCJcXG5cXG4gICAgKGVuZm9yY2VcXG4gICAgICAoPSAoZmxvb3IgYW1vdW50IE1JTklNVU1fUFJFQ0lTSU9OKVxcbiAgICAgICAgIGFtb3VudClcXG4gICAgICAoZm9ybWF0IFxcXCJBbW91bnQgdmlvbGF0ZXMgbWluaW11bSBwcmVjaXNpb246IHt9XFxcIiBbYW1vdW50XSkpXFxuICAgIClcXG5cXG4gIChkZWZ1biBlbmZvcmNlLWFjY291bnQgKGFjY291bnQ6c3RyaW5nKVxcbiAgICBAZG9jIFxcXCJFbmZvcmNlIHRoYXQgYW4gYWNjb3VudCBuYW1lIGNvbmZvcm1zIHRvIHRoZSBjb2luIGNvbnRyYWN0IFxcXFxcXG4gICAgICAgICBcXFxcbWluaW11bSBhbmQgbWF4aW11bSBsZW5ndGggcmVxdWlyZW1lbnRzLCBhcyB3ZWxsIGFzIHRoZSAgICBcXFxcXFxuICAgICAgICAgXFxcXGxhdGluLTEgY2hhcmFjdGVyIHNldC5cXFwiXFxuXFxuICAgIChlbmZvcmNlXFxuICAgICAgKGlzLWNoYXJzZXQgQ09JTl9DSEFSU0VUIGFjY291bnQpXFxuICAgICAgKGZvcm1hdFxcbiAgICAgICAgXFxcIkFjY291bnQgZG9lcyBub3QgY29uZm9ybSB0byB0aGUgY29pbiBjb250cmFjdCBjaGFyc2V0OiB7fVxcXCJcXG4gICAgICAgIFthY2NvdW50XSkpXFxuXFxuICAgIChsZXQgKChhY2NvdW50LWxlbmd0aCAobGVuZ3RoIGFjY291bnQpKSlcXG5cXG4gICAgICAoZW5mb3JjZVxcbiAgICAgICAgKD49IGFjY291bnQtbGVuZ3RoIE1JTklNVU1fQUNDT1VOVF9MRU5HVEgpXFxuICAgICAgICAoZm9ybWF0XFxuICAgICAgICAgIFxcXCJBY2NvdW50IG5hbWUgZG9lcyBub3QgY29uZm9ybSB0byB0aGUgbWluIGxlbmd0aCByZXF1aXJlbWVudDoge31cXFwiXFxuICAgICAgICAgIFthY2NvdW50XSkpXFxuXFxuICAgICAgKGVuZm9yY2VcXG4gICAgICAgICg8PSBhY2NvdW50LWxlbmd0aCBNQVhJTVVNX0FDQ09VTlRfTEVOR1RIKVxcbiAgICAgICAgKGZvcm1hdFxcbiAgICAgICAgICBcXFwiQWNjb3VudCBuYW1lIGRvZXMgbm90IGNvbmZvcm0gdG8gdGhlIG1pbiBsZW5ndGggcmVxdWlyZW1lbnQ6IHt9XFxcIlxcbiAgICAgICAgICBbYWNjb3VudF0pKVxcbiAgICAgIClcXG4gIClcXG5cXG4gIDsgLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS1cXG4gIDsgQ29pbiBDb250cmFjdFxcblxcbiAgKGRlZnVuIGJ1eS1nYXM6c3RyaW5nIChzZW5kZXI6c3RyaW5nIHRvdGFsOmRlY2ltYWwpXFxuICAgIEBkb2MgXFxcIlRoaXMgZnVuY3Rpb24gZGVzY3JpYmVzIHRoZSBtYWluICdnYXMgYnV5JyBvcGVyYXRpb24uIEF0IHRoaXMgcG9pbnQgXFxcXFxcbiAgICBcXFxcTUlORVIgaGFzIGJlZW4gY2hvc2VuIGZyb20gdGhlIHBvb2wsIGFuZCB3aWxsIGJlIHZhbGlkYXRlZC4gVGhlIFNFTkRFUiAgIFxcXFxcXG4gICAgXFxcXG9mIHRoaXMgdHJhbnNhY3Rpb24gaGFzIHNwZWNpZmllZCBhIGdhcyBsaW1pdCBMSU1JVCAobWF4aW11bSBnYXMpIGZvciAgICBcXFxcXFxuICAgIFxcXFx0aGUgdHJhbnNhY3Rpb24sIGFuZCB0aGUgcHJpY2UgaXMgdGhlIHNwb3QgcHJpY2Ugb2YgZ2FzIGF0IHRoYXQgdGltZS4gICAgXFxcXFxcbiAgICBcXFxcVGhlIGdhcyBidXkgd2lsbCBiZSBleGVjdXRlZCBwcmlvciB0byBleGVjdXRpbmcgU0VOREVSJ3MgY29kZS5cXFwiXFxuXFxuICAgIEBtb2RlbCBbIChwcm9wZXJ0eSAoPiB0b3RhbCAwLjApKVxcbiAgICAgICAgICAgICAocHJvcGVydHkgKGFjY291bnQgc2VuZGVyKSlcXG4gICAgICAgICAgIF1cXG5cXG4gICAgKGVuZm9yY2UtYWNjb3VudCBzZW5kZXIpXFxuXFxuICAgIChlbmZvcmNlLXVuaXQgdG90YWwpXFxuICAgIChlbmZvcmNlICg-IHRvdGFsIDAuMCkgXFxcImdhcyBzdXBwbHkgbXVzdCBiZSBhIHBvc2l0aXZlIHF1YW50aXR5XFxcIilcXG5cXG4gICAgKHJlcXVpcmUtY2FwYWJpbGl0eSAoRlVORF9UWCkpXFxuICAgICh3aXRoLWNhcGFiaWxpdHkgKFRSQU5TRkVSKVxcbiAgICAgIChkZWJpdCBzZW5kZXIgdG90YWwpKVxcbiAgICApXFxuXFxuICAoZGVmdW4gcmVkZWVtLWdhczpzdHJpbmcgKG1pbmVyOnN0cmluZyBtaW5lci1ndWFyZDpndWFyZCBzZW5kZXI6c3RyaW5nIHRvdGFsOmRlY2ltYWwpXFxuICAgIEBkb2MgXFxcIlRoaXMgZnVuY3Rpb24gZGVzY3JpYmVzIHRoZSBtYWluICdyZWRlZW0gZ2FzJyBvcGVyYXRpb24uIEF0IHRoaXMgICAgXFxcXFxcbiAgICBcXFxccG9pbnQsIHRoZSBTRU5ERVIncyB0cmFuc2FjdGlvbiBoYXMgYmVlbiBleGVjdXRlZCwgYW5kIHRoZSBnYXMgdGhhdCAgICAgIFxcXFxcXG4gICAgXFxcXHdhcyBjaGFyZ2VkIGhhcyBiZWVuIGNhbGN1bGF0ZWQuIE1JTkVSIHdpbGwgYmUgY3JlZGl0ZWQgdGhlIGdhcyBjb3N0LCAgICBcXFxcXFxuICAgIFxcXFxhbmQgU0VOREVSIHdpbGwgcmVjZWl2ZSB0aGUgcmVtYWluZGVyIHVwIHRvIHRoZSBsaW1pdFxcXCJcXG5cXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5ICg-IHRvdGFsIDAuMCkpXFxuICAgICAgICAgICAgIChwcm9wZXJ0eSAoYWNjb3VudCBzZW5kZXIpKVxcbiAgICAgICAgICAgICAocHJvcGVydHkgKGFjY291bnQgbWluZXIpKVxcbiAgICAgICAgICAgXVxcblxcbiAgICAoZW5mb3JjZS1hY2NvdW50IHNlbmRlcilcXG4gICAgKGVuZm9yY2UtYWNjb3VudCBtaW5lcilcXG4gICAgKGVuZm9yY2UtdW5pdCB0b3RhbClcXG5cXG4gICAgKHJlcXVpcmUtY2FwYWJpbGl0eSAoRlVORF9UWCkpXFxuICAgICh3aXRoLWNhcGFiaWxpdHkgKFRSQU5TRkVSKVxcbiAgICAgIChsZXQqICgoZmVlIChyZWFkLWRlY2ltYWwgXFxcImZlZVxcXCIpKVxcbiAgICAgICAgICAgICAocmVmdW5kICgtIHRvdGFsIGZlZSkpKVxcblxcbiAgICAgICAgKGVuZm9yY2UtdW5pdCBmZWUpXFxuXFxuICAgICAgICAoZW5mb3JjZSAoPj0gZmVlIDAuMClcXG4gICAgICAgICAgXFxcImZlZSBtdXN0IGJlIGEgbm9uLW5lZ2F0aXZlIHF1YW50aXR5XFxcIilcXG5cXG4gICAgICAgIChlbmZvcmNlICg-PSByZWZ1bmQgMC4wKVxcbiAgICAgICAgICBcXFwicmVmdW5kIG11c3QgYmUgYSBub24tbmVnYXRpdmUgcXVhbnRpdHlcXFwiKVxcblxcbiAgICAgICAgOyBkaXJlY3RseSB1cGRhdGUgaW5zdGVhZCBvZiBjcmVkaXRcXG4gICAgICAgIChpZiAoPiByZWZ1bmQgMC4wKVxcbiAgICAgICAgICAod2l0aC1yZWFkIGNvaW4tdGFibGUgc2VuZGVyXFxuICAgICAgICAgICAgeyBcXFwiYmFsYW5jZVxcXCIgOj0gYmFsYW5jZSB9XFxuICAgICAgICAgICAgKHVwZGF0ZSBjb2luLXRhYmxlIHNlbmRlclxcbiAgICAgICAgICAgICAgeyBcXFwiYmFsYW5jZVxcXCI6ICgrIGJhbGFuY2UgcmVmdW5kKSB9KVxcbiAgICAgICAgICAgIClcXG4gICAgICAgICAgXFxcIm5vb3BcXFwiKVxcblxcbiAgICAgICAgKGlmICg-IGZlZSAwLjApXFxuICAgICAgICAgIChjcmVkaXQgbWluZXIgbWluZXItZ3VhcmQgZmVlKVxcbiAgICAgICAgICBcXFwibm9vcFxcXCIpXFxuICAgICAgICApKVxcbiAgICApXFxuXFxuICAoZGVmdW4gY3JlYXRlLWFjY291bnQ6c3RyaW5nIChhY2NvdW50OnN0cmluZyBndWFyZDpndWFyZClcXG4gICAgQGRvYyBcXFwiQ3JlYXRlIGFuIGFjY291bnQgZm9yIEFDQ09VTlQsIHdpdGggR1VBUkQgY29udHJvbGxpbmcgYWNjZXNzIHRvIHRoZSAgXFxcXFxcbiAgICBcXFxcYWNjb3VudC5cXFwiXFxuXFxuICAgIEBtb2RlbCBbIChwcm9wZXJ0eSAoYWNjb3VudCBhY2NvdW50KSkgXVxcblxcbiAgICAoZW5mb3JjZS1hY2NvdW50IGFjY291bnQpXFxuXFxuICAgIChpbnNlcnQgY29pbi10YWJsZSBhY2NvdW50XFxuICAgICAgeyBcXFwiYmFsYW5jZVxcXCIgOiAwLjBcXG4gICAgICAsIFxcXCJndWFyZFxcXCIgICA6IGd1YXJkXFxuICAgICAgfSlcXG4gICAgKVxcblxcbiAgKGRlZnVuIGFjY291bnQtYmFsYW5jZTpkZWNpbWFsIChhY2NvdW50OnN0cmluZylcXG4gICAgQGRvYyBcXFwiQ2hlY2sgYW4gYWNjb3VudCdzIGJhbGFuY2UuXFxcIlxcblxcbiAgICBAbW9kZWwgWyAocHJvcGVydHkgKGFjY291bnQgYWNjb3VudCkpIF1cXG5cXG4gICAgKGVuZm9yY2UtYWNjb3VudCBhY2NvdW50KVxcblxcbiAgICAod2l0aC1yZWFkIGNvaW4tdGFibGUgYWNjb3VudFxcbiAgICAgIHsgXFxcImJhbGFuY2VcXFwiIDo9IGJhbGFuY2UgfVxcbiAgICAgIGJhbGFuY2VcXG4gICAgICApXFxuICAgIClcXG5cXG4gIChkZWZ1biBhY2NvdW50LWluZm86b2JqZWN0IChhY2NvdW50OnN0cmluZylcXG4gICAgQGRvYyBcXFwiR2V0IGFsbCBvZiBhbiBhY2NvdW50J3MgaW5mby4gIFRoaXMgaW5jbHVkZXMgdGhlIGJhbGFuY2UgYW5kIHRoZSAgICBcXFxcXFxuICAgIFxcXFxndWFyZC5cXFwiXFxuXFxuICAgIEBtb2RlbCBbIChwcm9wZXJ0eSAoYWNjb3VudCBhY2NvdW50KSkgXVxcblxcbiAgICAoZW5mb3JjZS1hY2NvdW50IGFjY291bnQpXFxuXFxuICAgIChyZWFkIGNvaW4tdGFibGUgYWNjb3VudClcXG4gICAgKVxcblxcbiAgKGRlZnVuIHJvdGF0ZS1ndWFyZDpzdHJpbmcgKGFjY291bnQ6c3RyaW5nIG5ldy1ndWFyZDpndWFyZClcXG4gICAgQGRvYyBcXFwiUm90YXRlIGd1YXJkIGFzc29jaWF0ZWQgd2l0aCBBQ0NPVU5UXFxcIlxcblxcbiAgICBAbW9kZWwgWyAocHJvcGVydHkgKGFjY291bnQgYWNjb3VudCkpIF1cXG5cXG4gICAgKGVuZm9yY2UtYWNjb3VudCBhY2NvdW50KVxcblxcbiAgICAod2l0aC1yZWFkIGNvaW4tdGFibGUgYWNjb3VudFxcbiAgICAgIHsgXFxcImd1YXJkXFxcIiA6PSBvbGQtZ3VhcmQgfVxcblxcbiAgICAgIChlbmZvcmNlLWd1YXJkIG9sZC1ndWFyZClcXG5cXG4gICAgICAodXBkYXRlIGNvaW4tdGFibGUgYWNjb3VudFxcbiAgICAgICAgeyBcXFwiZ3VhcmRcXFwiIDogbmV3LWd1YXJkIH1cXG4gICAgICAgICkpKVxcblxcblxcbiAgKGRlZnVuIHRyYW5zZmVyOnN0cmluZyAoc2VuZGVyOnN0cmluZyByZWNlaXZlcjpzdHJpbmcgYW1vdW50OmRlY2ltYWwpXFxuICAgIEBkb2MgXFxcIlRyYW5zZmVyIEFNT1VOVCBiZXR3ZWVuIGFjY291bnRzIFNFTkRFUiBhbmQgUkVDRUlWRVIgb24gdGhlIHNhbWUgICAgXFxcXFxcbiAgICBcXFxcY2hhaW4uIFRoaXMgZmFpbHMgaWYgZWl0aGVyIFNFTkRFUiBvciBSRUNFSVZFUiBkb2VzIG5vdCBleGlzdC4gICAgICAgICAgIFxcXFxcXG4gICAgXFxcXENyZWF0ZS1vbi10cmFuc2ZlciBjYW4gYmUgZG9uZSB1c2luZyB0aGUgJ3RyYW5zZmVyLWFuZC1jcmVhdGUnIGZ1bmN0aW9uLlxcXCJcXG5cXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5IGNvbnNlcnZlcy1tYXNzKVxcbiAgICAgICAgICAgICAocHJvcGVydHkgKD4gYW1vdW50IDAuMCkpXFxuICAgICAgICAgICAgIChwcm9wZXJ0eSAoYWNjb3VudCBzZW5kZXIpKVxcbiAgICAgICAgICAgICAocHJvcGVydHkgKGFjY291bnQgcmVjZWl2ZXIpKVxcbiAgICAgICAgICAgICAocHJvcGVydHkgKCE9IHNlbmRlciByZWNlaXZlcikpIF1cXG5cXG4gICAgKGVuZm9yY2UgKCE9IHNlbmRlciByZWNlaXZlcilcXG4gICAgICBcXFwic2VuZGVyIGNhbm5vdCBiZSB0aGUgcmVjZWl2ZXIgb2YgYSB0cmFuc2ZlclxcXCIpXFxuXFxuICAgIChlbmZvcmNlLWFjY291bnQgc2VuZGVyKVxcbiAgICAoZW5mb3JjZS1hY2NvdW50IHJlY2VpdmVyKVxcblxcbiAgICAoZW5mb3JjZSAoPiBhbW91bnQgMC4wKVxcbiAgICAgIFxcXCJ0cmFuc2ZlciBhbW91bnQgbXVzdCBiZSBwb3NpdGl2ZVxcXCIpXFxuXFxuICAgIChlbmZvcmNlLXVuaXQgYW1vdW50KVxcblxcbiAgICAod2l0aC1jYXBhYmlsaXR5IChUUkFOU0ZFUilcXG4gICAgICAoZGViaXQgc2VuZGVyIGFtb3VudClcXG4gICAgICAod2l0aC1yZWFkIGNvaW4tdGFibGUgcmVjZWl2ZXJcXG4gICAgICAgIHsgXFxcImd1YXJkXFxcIiA6PSBnIH1cXG5cXG4gICAgICAgIChjcmVkaXQgcmVjZWl2ZXIgZyBhbW91bnQpKVxcbiAgICAgIClcXG4gICAgKVxcblxcbiAgKGRlZnVuIHRyYW5zZmVyLWNyZWF0ZTpzdHJpbmdcXG4gICAgKCBzZW5kZXI6c3RyaW5nXFxuICAgICAgcmVjZWl2ZXI6c3RyaW5nXFxuICAgICAgcmVjZWl2ZXItZ3VhcmQ6Z3VhcmRcXG4gICAgICBhbW91bnQ6ZGVjaW1hbCApXFxuXFxuICAgIEBkb2MgXFxcIlRyYW5zZmVyIGJldHdlZW4gYWNjb3VudHMgU0VOREVSIGFuZCBSRUNFSVZFUiBvbiB0aGUgc2FtZSBjaGFpbi4gICAgXFxcXFxcbiAgICBcXFxcVGhpcyBmYWlscyBpZiB0aGUgU0VOREVSIGFjY291bnQgZG9lcyBub3QgZXhpc3QuIElmIHRoZSBSRUNFSVZFUiBhY2NvdW50IFxcXFxcXG4gICAgXFxcXGRvZXMgbm90IGV4aXN0LCBpdCBpcyBjcmVhdGVkIGFuZCBhc3NvY2lhdGVkIHdpdGggR1VBUkQuXFxcIlxcblxcbiAgICBAbW9kZWwgWyA7KHByb3BlcnR5IGNvbnNlcnZlcy1tYXNzKSA7OyBmYWlscyBvbiBtaXNzaW5nIHJvdywgRlYgcHJvYmxlbVxcbiAgICAgICAgICAgIChwcm9wZXJ0eSAoPiBhbW91bnQgMC4wKSlcXG4gICAgICAgICAgICAocHJvcGVydHkgKGFjY291bnQgc2VuZGVyKSlcXG4gICAgICAgICAgICAocHJvcGVydHkgKGFjY291bnQgcmVjZWl2ZXIpKVxcbiAgICAgICAgICAgIChwcm9wZXJ0eSAoIT0gc2VuZGVyIHJlY2VpdmVyKSkgXVxcblxcbiAgICAoZW5mb3JjZSAoIT0gc2VuZGVyIHJlY2VpdmVyKVxcbiAgICAgIFxcXCJzZW5kZXIgY2Fubm90IGJlIHRoZSByZWNlaXZlciBvZiBhIHRyYW5zZmVyXFxcIilcXG5cXG4gICAgKGVuZm9yY2UtYWNjb3VudCBzZW5kZXIpXFxuICAgIChlbmZvcmNlLWFjY291bnQgcmVjZWl2ZXIpXFxuXFxuICAgIChlbmZvcmNlICg-IGFtb3VudCAwLjApXFxuICAgICAgXFxcInRyYW5zZmVyIGFtb3VudCBtdXN0IGJlIHBvc2l0aXZlXFxcIilcXG5cXG4gICAgKGVuZm9yY2UtdW5pdCBhbW91bnQpXFxuXFxuICAgICh3aXRoLWNhcGFiaWxpdHkgKFRSQU5TRkVSKVxcbiAgICAgIChkZWJpdCBzZW5kZXIgYW1vdW50KVxcbiAgICAgIChjcmVkaXQgcmVjZWl2ZXIgcmVjZWl2ZXItZ3VhcmQgYW1vdW50KSlcXG4gICAgKVxcblxcbiAgKGRlZnVuIGNvaW5iYXNlOnN0cmluZyAoYWNjb3VudDpzdHJpbmcgYWNjb3VudC1ndWFyZDpndWFyZCBhbW91bnQ6ZGVjaW1hbClcXG4gICAgQGRvYyBcXFwiSW50ZXJuYWwgZnVuY3Rpb24gZm9yIHRoZSBpbml0aWFsIGNyZWF0aW9uIG9mIGNvaW5zLiAgVGhpcyBmdW5jdGlvbiBcXFxcXFxuICAgIFxcXFxjYW5ub3QgYmUgdXNlZCBvdXRzaWRlIG9mIHRoZSBjb2luIGNvbnRyYWN0LlxcXCJcXG5cXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5IChhY2NvdW50IGFjY291bnQpKSBdXFxuXFxuICAgIChlbmZvcmNlLWFjY291bnQgYWNjb3VudClcXG4gICAgKGVuZm9yY2UtdW5pdCBhbW91bnQpXFxuXFxuICAgIChyZXF1aXJlLWNhcGFiaWxpdHkgKENPSU5CQVNFKSlcXG4gICAgKHdpdGgtY2FwYWJpbGl0eSAoVFJBTlNGRVIpXFxuICAgICAgKGNyZWRpdCBhY2NvdW50IGFjY291bnQtZ3VhcmQgYW1vdW50KSlcXG4gICAgKVxcblxcbiAgKGRlZnBhY3QgZnVuZC10eCAoc2VuZGVyOnN0cmluZyBtaW5lcjpzdHJpbmcgbWluZXItZ3VhcmQ6Z3VhcmQgdG90YWw6ZGVjaW1hbClcXG4gICAgQGRvYyBcXFwiJ2Z1bmQtdHgnIGlzIGEgc3BlY2lhbCBwYWN0IHRvIGZ1bmQgYSB0cmFuc2FjdGlvbiBpbiB0d28gc3RlcHMsICAgICBcXFxcXFxuICAgIFxcXFx3aXRoIHRoZSBhY3R1YWwgdHJhbnNhY3Rpb24gdHJhbnNwaXJpbmcgaW4gdGhlIG1pZGRsZTogICAgICAgICAgICAgICAgICAgXFxcXFxcbiAgICBcXFxcICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFxcXFxcXG4gICAgXFxcXCAgMSkgQSBidXlpbmcgcGhhc2UsIGRlYml0aW5nIHRoZSBzZW5kZXIgZm9yIHRvdGFsIGdhcyBhbmQgZmVlLCB5aWVsZGluZyBcXFxcXFxuICAgIFxcXFwgICAgIFRYX01BWF9DSEFSR0UuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXFxcXFxcbiAgICBcXFxcICAyKSBBIHNldHRsZW1lbnQgcGhhc2UsIHJlc3VtaW5nIFRYX01BWF9DSEFSR0UsIGFuZCBhbGxvY2F0aW5nIHRvIHRoZSAgIFxcXFxcXG4gICAgXFxcXCAgICAgY29pbmJhc2UgYWNjb3VudCBmb3IgdXNlZCBnYXMgYW5kIGZlZSwgYW5kIHNlbmRlciBhY2NvdW50IGZvciBiYWwtICBcXFxcXFxuICAgIFxcXFwgICAgIGFuY2UgKHVudXNlZCBnYXMsIGlmIGFueSkuXFxcIlxcblxcbiAgICBAbW9kZWwgWyAocHJvcGVydHkgKD4gdG90YWwgMC4wKSlcXG4gICAgICAgICAgICAgKHByb3BlcnR5IChhY2NvdW50IHNlbmRlcikpXFxuICAgICAgICAgICAgIChwcm9wZXJ0eSAoYWNjb3VudCBtaW5lcikpXFxuICAgICAgICAgICAgIDsocHJvcGVydHkgY29uc2VydmVzLW1hc3MpIG5vdCBzdXBwb3J0ZWQgeWV0XFxuICAgICAgICAgICBdXFxuXFxuICAgIChzdGVwIChidXktZ2FzIHNlbmRlciB0b3RhbCkpXFxuICAgIChzdGVwIChyZWRlZW0tZ2FzIG1pbmVyIG1pbmVyLWd1YXJkIHNlbmRlciB0b3RhbCkpXFxuICAgIClcXG5cXG4gIChkZWZ1biBkZWJpdDpzdHJpbmcgKGFjY291bnQ6c3RyaW5nIGFtb3VudDpkZWNpbWFsKVxcbiAgICBAZG9jIFxcXCJEZWJpdCBBTU9VTlQgZnJvbSBBQ0NPVU5UIGJhbGFuY2VcXFwiXFxuXFxuICAgIEBtb2RlbCBbIChwcm9wZXJ0eSAoPiBhbW91bnQgMC4wKSlcXG4gICAgICAgICAgICAgKHByb3BlcnR5IChhY2NvdW50IGFjY291bnQpKVxcbiAgICAgICAgICAgXVxcblxcbiAgICAoZW5mb3JjZS1hY2NvdW50IGFjY291bnQpXFxuXFxuICAgIChlbmZvcmNlICg-IGFtb3VudCAwLjApXFxuICAgICAgXFxcImRlYml0IGFtb3VudCBtdXN0IGJlIHBvc2l0aXZlXFxcIilcXG5cXG4gICAgKGVuZm9yY2UtdW5pdCBhbW91bnQpXFxuXFxuICAgIChyZXF1aXJlLWNhcGFiaWxpdHkgKFRSQU5TRkVSKSlcXG4gICAgKHdpdGgtY2FwYWJpbGl0eSAoQUNDT1VOVF9HVUFSRCBhY2NvdW50KVxcbiAgICAgICh3aXRoLXJlYWQgY29pbi10YWJsZSBhY2NvdW50XFxuICAgICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6PSBiYWxhbmNlIH1cXG5cXG4gICAgICAgIChlbmZvcmNlICg8PSBhbW91bnQgYmFsYW5jZSkgXFxcIkluc3VmZmljaWVudCBmdW5kc1xcXCIpXFxuICAgICAgICAodXBkYXRlIGNvaW4tdGFibGUgYWNjb3VudFxcbiAgICAgICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6ICgtIGJhbGFuY2UgYW1vdW50KSB9XFxuICAgICAgICAgICkpKVxcbiAgICApXFxuXFxuXFxuICAoZGVmdW4gY3JlZGl0OnN0cmluZyAoYWNjb3VudDpzdHJpbmcgZ3VhcmQ6Z3VhcmQgYW1vdW50OmRlY2ltYWwpXFxuICAgIEBkb2MgXFxcIkNyZWRpdCBBTU9VTlQgdG8gQUNDT1VOVCBiYWxhbmNlXFxcIlxcblxcbiAgICBAbW9kZWwgWyAocHJvcGVydHkgKD4gYW1vdW50IDAuMCkpXFxuICAgICAgICAgICAgIChwcm9wZXJ0eSAoYWNjb3VudCBhY2NvdW50KSlcXG4gICAgICAgICAgIF1cXG5cXG4gICAgKGVuZm9yY2UtYWNjb3VudCBhY2NvdW50KVxcblxcbiAgICAoZW5mb3JjZSAoPiBhbW91bnQgMC4wKSBcXFwiY3JlZGl0IGFtb3VudCBtdXN0IGJlIHBvc2l0aXZlXFxcIilcXG4gICAgKGVuZm9yY2UtdW5pdCBhbW91bnQpXFxuXFxuICAgIChyZXF1aXJlLWNhcGFiaWxpdHkgKFRSQU5TRkVSKSlcXG4gICAgKHdpdGgtZGVmYXVsdC1yZWFkIGNvaW4tdGFibGUgYWNjb3VudFxcbiAgICAgIHsgXFxcImJhbGFuY2VcXFwiIDogMC4wLCBcXFwiZ3VhcmRcXFwiIDogZ3VhcmQgfVxcbiAgICAgIHsgXFxcImJhbGFuY2VcXFwiIDo9IGJhbGFuY2UsIFxcXCJndWFyZFxcXCIgOj0gcmV0ZyB9XFxuICAgICAgOyB3ZSBkb24ndCB3YW50IHRvIG92ZXJ3cml0ZSBhbiBleGlzdGluZyBndWFyZCB3aXRoIHRoZSB1c2VyLXN1cHBsaWVkIG9uZVxcbiAgICAgIChlbmZvcmNlICg9IHJldGcgZ3VhcmQpXFxuICAgICAgICBcXFwiYWNjb3VudCBndWFyZHMgZG8gbm90IG1hdGNoXFxcIilcXG5cXG4gICAgICAod3JpdGUgY29pbi10YWJsZSBhY2NvdW50XFxuICAgICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6ICgrIGJhbGFuY2UgYW1vdW50KVxcbiAgICAgICAgLCBcXFwiZ3VhcmRcXFwiICAgOiByZXRnXFxuICAgICAgICB9KVxcbiAgICAgICkpXFxuXFxuICAoZGVmcGFjdCBjcm9zcy1jaGFpbi10cmFuc2ZlclxcbiAgICAoIGRlbGV0ZS1hY2NvdW50OnN0cmluZ1xcbiAgICAgIGNyZWF0ZS1jaGFpbi1pZDpzdHJpbmdcXG4gICAgICBjcmVhdGUtYWNjb3VudDpzdHJpbmdcXG4gICAgICBjcmVhdGUtYWNjb3VudC1ndWFyZDpndWFyZFxcbiAgICAgIHF1YW50aXR5OmRlY2ltYWwgKVxcblxcbiAgICBAZG9jIFxcXCJUcmFuc2ZlciBRVUFOVElUWSBjb2lucyBmcm9tIERFTEVURS1BQ0NPVU5UIG9uIGN1cnJlbnQgY2hhaW4gdG8gICAgICAgICAgIFxcXFxcXG4gICAgICAgICBcXFxcQ1JFQVRFLUFDQ09VTlQgb24gQ1JFQVRFLUNIQUlOLUlELiBUYXJnZXQgY2hhaW4gaWQgbXVzdCBub3QgYmUgdGhlICAgICAgICBcXFxcXFxuICAgICAgICAgXFxcXGN1cnJlbnQgY2hhaW4taWQuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXFxcXFxcbiAgICAgICAgIFxcXFwgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFxcXFxcXG4gICAgICAgICBcXFxcU3RlcCAxOiBCdXJuIFFVQU5USVRZLW1hbnkgY29pbnMgZm9yIERFTEVURS1BQ0NPVU5UIG9uIHRoZSBjdXJyZW50IGNoYWluLCBcXFxcXFxuICAgICAgICAgXFxcXGFuZCBwcm9kdWNlIGFuIFNQViByZWNlaXB0IHdoaWNoIG1heSBiZSBtYW51YWxseSByZWRlZW1lZCBmb3IgYW4gU1BWICAgICAgXFxcXFxcbiAgICAgICAgIFxcXFxwcm9vZi4gT25jZSBhIHByb29mIGlzIG9idGFpbmVkLCB0aGUgdXNlciBtYXkgY2FsbCAnY3JlYXRlLWNvaW4nIGFuZCAgICAgIFxcXFxcXG4gICAgICAgICBcXFxcY29uc3VtZSB0aGUgcHJvb2Ygb24gQ1JFQVRFLUNIQUlOLUlELCBjcmVkaXRpbmcgQ1JFQVRFLUFDQ09VTlQgUVVBTlRJVFktICBcXFxcXFxuICAgICAgICAgXFxcXG1hbnkgY29pbnMuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXFxcXFxcbiAgICAgICAgIFxcXFwgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFxcXFxcXG4gICAgICAgICBcXFxcU3RlcCAyOiBDb25zdW1lIGFuIFNQViBwcm9vZiBmb3IgYSBudW1iZXIgb2YgY29pbnMsIGFuZCBjcmVkaXQgdGhlICAgICAgICBcXFxcXFxuICAgICAgICAgXFxcXGFjY291bnQgYXNzb2NpYXRlZCB3aXRoIHRoZSBwcm9vZiB0aGUgcXVhbnRpZnkgb2YgY29pbnMgYnVybmVkIG9uIHRoZSAgICAgXFxcXFxcbiAgICAgICAgIFxcXFxzb3VyY2UgY2hhaW4gYnkgdGhlIGJ1cm4gYWNjb3VudC4gTm90ZTogbXVzdCBiZSBjYWxsZWQgb24gdGhlIGNvcnJlY3QgICAgIFxcXFxcXG4gICAgICAgICBcXFxcY2hhaW4gaWQgYXMgc3BlY2lmaWVkIGluIHRoZSBwcm9vZi5cXFwiXFxuXFxuICAgIEBtb2RlbCBbIChwcm9wZXJ0eSAoPiBxdWFudGl0eSAwLjApKVxcbiAgICAgICAgICAgICAocHJvcGVydHkgKCE9IGNyZWF0ZS1jaGFpbi1pZCBcXFwiXFxcIikpXFxuICAgICAgICAgICAgIChwcm9wZXJ0eSAoYWNjb3VudCBkZWxldGUtYWNjb3VudCkpXFxuICAgICAgICAgICAgIChwcm9wZXJ0eSAoYWNjb3VudCBjcmVhdGUtYWNjb3VudCkpXFxuICAgICAgICAgICBdXFxuXFxuICAgIChzdGVwXFxuICAgICAgKHdpdGgtY2FwYWJpbGl0eSAoVFJBTlNGRVIpXFxuXFxuICAgICAgICAoZW5mb3JjZS1hY2NvdW50IGRlbGV0ZS1hY2NvdW50KVxcbiAgICAgICAgKGVuZm9yY2UtYWNjb3VudCBjcmVhdGUtYWNjb3VudClcXG5cXG4gICAgICAgIChlbmZvcmNlICghPSBcXFwiXFxcIiBjcmVhdGUtY2hhaW4taWQpIFxcXCJlbXB0eSBjcmVhdGUtY2hhaW4taWRcXFwiKVxcbiAgICAgICAgKGVuZm9yY2UgKCE9IChhdCAnY2hhaW4taWQgKGNoYWluLWRhdGEpKSBjcmVhdGUtY2hhaW4taWQpXFxuICAgICAgICAgIFxcXCJjYW5ub3QgcnVuIGNyb3NzLWNoYWluIHRyYW5zZmVycyB0byB0aGUgc2FtZSBjaGFpblxcXCIpXFxuXFxuICAgICAgICAoZW5mb3JjZSAoPiBxdWFudGl0eSAwLjApXFxuICAgICAgICAgIFxcXCJ0cmFuc2ZlciBxdWFudGl0eSBtdXN0IGJlIHBvc2l0aXZlXFxcIilcXG5cXG4gICAgICAgIChlbmZvcmNlLXVuaXQgcXVhbnRpdHkpXFxuXFxuICAgICAgICA7OyBzdGVwIDEgLSBkZWJpdCBkZWxldGUtYWNjb3VudCBvbiBjdXJyZW50IGNoYWluXFxuICAgICAgICAoZGViaXQgZGVsZXRlLWFjY291bnQgcXVhbnRpdHkpXFxuXFxuICAgICAgICAobGV0XFxuICAgICAgICAgICgocmV0djpvYmplY3R7dHJhbnNmZXItc2NoZW1hfVxcbiAgICAgICAgICAgIHsgXFxcImNyZWF0ZS1hY2NvdW50XFxcIiA6IGNyZWF0ZS1hY2NvdW50XFxuICAgICAgICAgICAgLCBcXFwiY3JlYXRlLWFjY291bnQtZ3VhcmRcXFwiIDogY3JlYXRlLWFjY291bnQtZ3VhcmRcXG4gICAgICAgICAgICAsIFxcXCJxdWFudGl0eVxcXCIgOiBxdWFudGl0eVxcbiAgICAgICAgICAgIH0pKVxcbiAgICAgICAgICAoeWllbGQgcmV0diBjcmVhdGUtY2hhaW4taWQpXFxuICAgICAgICAgICkpKVxcblxcbiAgICAoc3RlcFxcbiAgICAgIChyZXN1bWVcXG4gICAgICAgIHsgXFxcImNyZWF0ZS1hY2NvdW50XFxcIiA6PSBjcmVhdGUtYWNjb3VudFxcbiAgICAgICAgLCBcXFwiY3JlYXRlLWFjY291bnQtZ3VhcmRcXFwiIDo9IGNyZWF0ZS1hY2NvdW50LWd1YXJkXFxuICAgICAgICAsIFxcXCJxdWFudGl0eVxcXCIgOj0gcXVhbnRpdHlcXG4gICAgICAgIH1cXG5cXG4gICAgICAgIDs7IHN0ZXAgMiAtIGNyZWRpdCBjcmVhdGUgYWNjb3VudCBvbiB0YXJnZXQgY2hhaW5cXG4gICAgICAgICh3aXRoLWNhcGFiaWxpdHkgKFRSQU5TRkVSKVxcbiAgICAgICAgICAoY3JlZGl0IGNyZWF0ZS1hY2NvdW50IGNyZWF0ZS1hY2NvdW50LWd1YXJkIHF1YW50aXR5KSlcXG4gICAgICAgICkpXFxuICAgIClcXG5cXG5cXG4gIDsgLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS1cXG4gIDsgQ29pbiBhbGxvY2F0aW9uc1xcblxcbiAgKGRlZnNjaGVtYSBhbGxvY2F0aW9uLXNjaGVtYVxcbiAgICBAZG9jIFxcXCJUaGUgY29pbiBhbGxvY2F0aW9uIHNjaGVtYSBmb3IgZ2VuZXNpcyBsb2NrdXBzXFxcIlxcbiAgICBAbW9kZWwgWyAoaW52YXJpYW50ICg-PSBiYWxhbmNlIDAuMCkpIF1cXG5cXG4gICAgYmFsYW5jZTpkZWNpbWFsXFxuICAgIGRhdGU6dGltZVxcbiAgICBndWFyZDpndWFyZFxcbiAgICByZWRlZW1lZDpib29sKVxcblxcbiAgKGRlZnRhYmxlIGFsbG9jYXRpb24tdGFibGU6e2FsbG9jYXRpb24tc2NoZW1hfSlcXG5cXG4gIChkZWZ1biBjcmVhdGUtYWxsb2NhdGlvbi1hY2NvdW50IChhY2NvdW50OnN0cmluZyBkYXRlOnRpbWUgZ3VhcmQtcmVmOnN0cmluZyBhbW91bnQ6ZGVjaW1hbClcXG4gICAgQGRvYyBcXFwiQWRkIGFuIGVudHJ5IHRvIHRoZSBjb2luIGFsbG9jYXRpb24gdGFibGVcXFwiXFxuICAgIEBtb2RlbCBbIChwcm9wZXJ0eSAoYWNjb3VudC1zdHJ1Y3R1cmUgYWNjb3VudCkpIF1cXG5cXG4gICAgKHdpdGgtY2FwYWJpbGl0eSAoR0VORVNJUylcXG4gICAgICAocmVxdWlyZS1jYXBhYmlsaXR5IChDT0lOQkFTRSkpXFxuXFxuICAgICAgKGVuZm9yY2UtYWNjb3VudCBhY2NvdW50KVxcbiAgICAgIChlbmZvcmNlICg-PSBhbW91bnQgMC4wKVxcbiAgICAgICAgXFxcImFsbG9jYXRpb24gYW1vdW50IG11c3QgYmUgbm9uLW5lZ2F0aXZlXFxcIilcXG5cXG4gICAgICAoZW5mb3JjZS11bml0IGFtb3VudClcXG5cXG4gICAgICAoaW5zZXJ0IGFsbG9jYXRpb24tdGFibGUgYWNjb3VudFxcbiAgICAgICAgeyBcXFwiYmFsYW5jZVxcXCIgOiBhbW91bnRcXG4gICAgICAgICwgXFxcImRhdGVcXFwiIDogZGF0ZVxcbiAgICAgICAgLCBcXFwiZ3VhcmRcXFwiIDogKGtleXNldC1yZWYtZ3VhcmQgZ3VhcmQtcmVmKVxcbiAgICAgICAgLCBcXFwicmVkZWVtZWRcXFwiIDogZmFsc2VcXG4gICAgICAgIH0pKSlcXG5cXG4gIChkZWZ1biByZWxlYXNlLWFsbG9jYXRpb25cXG4gICAgKCBhY2NvdW50OnN0cmluZ1xcbiAgICAgIHJlY2VpdmVyOnN0cmluZ1xcbiAgICAgIHJlY2VpdmVyLWd1YXJkOmd1YXJkXFxuICAgICAgYW1vdW50OmRlY2ltYWwgKVxcblxcbiAgICBAZG9jIFxcXCJSZWxlYXNlIGZ1bmRzIGFzc29jaWF0ZWQgd2l0aCBhbiBhbGxvY2F0aW9uIGFjY291bnRcXFwiXFxuICAgIEBtb2RlbFxcbiAgICAgIFsgKHByb3BlcnR5IChhY2NvdW50LXN0cnVjdHVyZSBhY2NvdW50KSlcXG4gICAgICAgIChwcm9wZXJ0eSAoPiBhbW91bnQgMC4wKSlcXG4gICAgICBdXFxuXFxuICAgIChlbmZvcmNlLWFjY291bnQgYWNjb3VudClcXG5cXG4gICAgKGVuZm9yY2UtdW5pdCBhbW91bnQpXFxuICAgIChlbmZvcmNlXFxuICAgICAgKD4gYW1vdW50IDAuMClcXG4gICAgICAoZm9ybWF0XFxuICAgICAgICBcXFwiYW1vdW50IHJlcXVlc3RlZCBmb3IgdW5sb2NrIGlzIG5vdCBhIHZhbGlkIHF1YW50aXR5OiB7fVxcXCJcXG4gICAgICAgIFthbW91bnRdKSlcXG5cXG4gICAgKHdpdGgtcmVhZCBhbGxvY2F0aW9uLXRhYmxlIGFjY291bnRcXG4gICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6PSBiYWxhbmNlXFxuICAgICAgLCBcXFwiZGF0ZVxcXCIgOj0gcmVsZWFzZS10aW1lXFxuICAgICAgLCBcXFwicmVkZWVtZWRcXFwiIDo9IHJlZGVlbWVkXFxuICAgICAgLCBcXFwiZ3VhcmRcXFwiIDo9IGd1YXJkXFxuICAgICAgfVxcblxcbiAgICAgIChsZXRcXG4gICAgICAgICgobmV3LWJhbGFuY2U6ZGVjaW1hbCAoLSBiYWxhbmNlIGFtb3VudCkpXFxuICAgICAgICAgKGN1cnItdGltZTp0aW1lIChhdCAnYmxvY2stdGltZSAoY2hhaW4tZGF0YSkpKSlcXG5cXG4gICAgICAgIChlbmZvcmNlIChub3QgcmVkZWVtZWQpXFxuICAgICAgICAgIFxcXCJhbGxvY2F0aW9uIGZ1bmRzIGhhdmUgYWxyZWFkeSBiZWVuIHJlZGVlbWVkXFxcIilcXG5cXG4gICAgICAgIChlbmZvcmNlXFxuICAgICAgICAgICg-PSAoZGlmZi10aW1lIHJlbGVhc2UtdGltZSBjdXJyLXRpbWUpIDAuMClcXG4gICAgICAgICAgKGZvcm1hdCBcXFwiZnVuZHMgbG9ja2VkIHVudGlsIHt9XFxcIiBbcmVsZWFzZS10aW1lXSkpXFxuXFxuICAgICAgICAoZW5mb3JjZS1ndWFyZCBndWFyZClcXG5cXG4gICAgICAgIChlbmZvcmNlXFxuICAgICAgICAgICg-PSBuZXctYmFsYW5jZSAwLjApXFxuICAgICAgICAgIFxcXCJpbnN1ZmZpY2llbnQgZnVuZHNcXFwiKVxcblxcbiAgICAgICAgOyB1cGRhdGUgYmFsYW5jZSB0byByZWZsZWN0IGNvaW5iYXNlXFxuICAgICAgICAodXBkYXRlIGFsbG9jYXRpb24tdGFibGUgYWNjb3VudFxcbiAgICAgICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6IG5ldy1iYWxhbmNlIH0pXFxuXFxuICAgICAgICAod2l0aC1jYXBhYmlsaXR5IChUUkFOU0ZFUilcXG4gICAgICAgICAgOyByZWxlYXNlIGZ1bmRzIHZpYSBjb2luYmFzZSB0byBhY2NvdW50XFxuICAgICAgICAgIChjcmVkaXQgcmVjZWl2ZXIgcmVjZWl2ZXItZ3VhcmQgYW1vdW50KSlcXG5cXG4gICAgICAgIDsgaWYgYWNjb3VudCBpcyBub3cgZW1wdHksIG1hcmsgcm93IGFzIHJlZGVlbWRcXG4gICAgICAgIChpZiAoPSBuZXctYmFsYW5jZSAwLjApXFxuICAgICAgICAgICh1cGRhdGUgYWxsb2NhdGlvbi10YWJsZSBhY2NvdW50XFxuICAgICAgICAgICAgeyBcXFwicmVkZWVtZWRcXFwiIDogdHJ1ZSB9KVxcblxcbiAgICAgICAgICBcXFwibm9vcFxcXCIpXFxuXFxuICAgICAgICBcXFwiQWxsb2NhdGlvbiBzdWNjZXNzZnVsXFxcIlxcbiAgICApKSlcXG5cXG4pXFxuXFxuKGNyZWF0ZS10YWJsZSBjb2luLXRhYmxlKVxcbihjcmVhdGUtdGFibGUgYWxsb2NhdGlvbi10YWJsZSlcXG5cIn19LFwic2lnbmVyc1wiOltdLFwibWV0YVwiOntcImNyZWF0aW9uVGltZVwiOjAsXCJ0dGxcIjoxNzI4MDAsXCJnYXNMaW1pdFwiOjAsXCJjaGFpbklkXCI6XCJcIixcImdhc1ByaWNlXCI6MCxcInNlbmRlclwiOlwiXCJ9LFwibm9uY2VcIjpcIlxcXCJnZW5lc2lzLTAxXFxcIlwifSJ9
  - eyJnYXMiOjAsInJlc3VsdCI6eyJzdGF0dXMiOiJzdWNjZXNzIiwiZGF0YSI6IlRhYmxlQ3JlYXRlZCJ9LCJyZXFLZXkiOiJEY2Ztc2FYS3BPWjJ6aDlCZXpaa2x5WVpiaG9qYVM2U0FrWVV6Q1Rld1dVIiwibG9ncyI6Il9mSGpYVTB5czJhQUVfZ2FjaGlub0s4WU1GTGEzSzhnWVJuLTdnbGNXRUkiLCJtZXRhRGF0YSI6bnVsbCwiY29udGludWF0aW9uIjpudWxsLCJ0eElkIjowfQ
- - eyJoYXNoIjoickcyblhicEg1R3lsU01rek04ZzhzTGVWOEtDODZ5cE9USlpGZ29QVGRHZyIsInNpZ3MiOltdLCJjbWQiOiJ7XCJuZXR3b3JrSWRcIjpudWxsLFwicGF5bG9hZFwiOntcImV4ZWNcIjp7XCJkYXRhXCI6e1wic2VuZGVyMDdcIjpbXCI0YzMxZGM5ZWU3ZjI0MTc3Zjc4YjZmNTE4MDEyYTIwODMyNmUyYWYxZjM3YmIwYTI0MDViNTA1NmQwY2FkNjI4XCJdLFwic2VuZGVyMDFcIjpbXCI2YmUyZjQ4NWE3YWY3NWZlZGI0YjdmMTUzYTkwM2Y3ZTYwMDBjYTRhYTUwMTE3OWM5MWEyNDUwYjc3N2JkMmE3XCJdLFwic2VuZGVyMDZcIjpbXCI1ZmZjMWY3ZmVmN2E0NDczODYyNTc2MmY3NWE0MjI5NDU0OTUxZTAzZjJhZmM2ZjgxMzA5YzBjMWJkZjllZTZmXCJdLFwic2VuZGVyMDBcIjpbXCIzNjg4MjBmODBjMzI0YmJjN2MyYjA2MTA2ODhhN2RhNDNlMzlmOTFkMTE4NzMyNjcxY2Q5Yzc1MDBmZjQzY2NhXCJdLFwiY3JvZXN1c1wiOltcIjI5OTNmNzk1ZDEzM2ZhNWQwZmQ4NzdhNjQxY2FiYzhiMjhjZDM2MTQ3ZjY2Njk4OGNhY2JhYTQzNzlkMWZmOTNcIl0sXCJzZW5kZXIwNVwiOltcImYwOWQ4ZjYzOTRhZWE0MjVmZTY3ODNkODhjZDgxMzYzZDgwMTdmMTZhZmQzNzExYzU3NWJlMGY1Y2Q1YzliYjlcIl0sXCJzZW5kZXIwNFwiOltcIjJkNzBhYTRmNjk3YzNhM2I4ZGQ2ZDk3NzQ1YWMwNzRlZGNmZDBlYjY1YzM3Nzc0Y2RlMjUxMzU0ODNiZWE3MWVcIl0sXCJtdWx0aS0wMi0wMy0wNC1hbnlcIjp7XCJwcmVkXCI6XCJrZXlzLWFueVwiLFwia2V5c1wiOltcIjNhOWRkNTMyZDczZGFjZTE5NWRiYjY0ZDFkYmE2NTcyZmI3ODNkMGZkZDMyNDY4NWUzMmZiZGEyZjg5Zjk5YTZcIixcIjQzZjJhZGIxZGUxOTIwMDBjYjM3NzdiYWNjN2Y5ODNiNjYxNGZkOWMxNzE1Y2Q0NGNkNDg0YjZkM2EwZDM0YzhcIixcIjJkNzBhYTRmNjk3YzNhM2I4ZGQ2ZDk3NzQ1YWMwNzRlZGNmZDBlYjY1YzM3Nzc0Y2RlMjUxMzU0ODNiZWE3MWVcIl19LFwic2VuZGVyMDlcIjpbXCJjNTlkOTg0MGIwYjY2MDkwODM2NTQ2YjdlYjRhNzM2MDYyNTc1MjdlYzhjMmI0ODIzMDBmZDIyOTI2NGIwN2U2XCJdLFwic2VuZGVyMDNcIjpbXCI0M2YyYWRiMWRlMTkyMDAwY2IzNzc3YmFjYzdmOTgzYjY2MTRmZDljMTcxNWNkNDRjZDQ4NGI2ZDNhMGQzNGM4XCJdLFwibXVsdGktMDAtMDFcIjpbXCIzNjg4MjBmODBjMzI0YmJjN2MyYjA2MTA2ODhhN2RhNDNlMzlmOTFkMTE4NzMyNjcxY2Q5Yzc1MDBmZjQzY2NhXCIsXCI2YmUyZjQ4NWE3YWY3NWZlZGI0YjdmMTUzYTkwM2Y3ZTYwMDBjYTRhYTUwMTE3OWM5MWEyNDUwYjc3N2JkMmE3XCJdLFwic2VuZGVyMDhcIjpbXCI2M2IyZWJhNGVkNzBkNDYxMmQzZTdiYzkwZGIyZmJmNGM3NmY3YjA3NDM2M2U4NmQ3M2YwYmM2MTdmOGU4YjgxXCJdLFwic2VuZGVyMDJcIjpbXCIzYTlkZDUzMmQ3M2RhY2UxOTVkYmI2NGQxZGJhNjU3MmZiNzgzZDBmZGQzMjQ2ODVlMzJmYmRhMmY4OWY5OWE2XCJdfSxcImNvZGVcIjpcIihjb2luLmNvaW5iYXNlIFxcXCJjcm9lc3VzXFxcIiAocmVhZC1rZXlzZXQgXFxcImNyb2VzdXNcXFwiKSA5MDAwMDAwMDAwLjApXFxuKGNvaW4uY29pbmJhc2UgXFxcInNlbmRlcjAwXFxcIiAocmVhZC1rZXlzZXQgXFxcInNlbmRlcjAwXFxcIikgMTAwMDAwMDAwLjApXFxuKGNvaW4uY29pbmJhc2UgXFxcInNlbmRlcjAxXFxcIiAocmVhZC1rZXlzZXQgXFxcInNlbmRlcjAxXFxcIikgMTEwMDAwMDAwLjApXFxuKGNvaW4uY29pbmJhc2UgXFxcInNlbmRlcjAyXFxcIiAocmVhZC1rZXlzZXQgXFxcInNlbmRlcjAyXFxcIikgMTIwMDAwMDAwLjApXFxuKGNvaW4uY29pbmJhc2UgXFxcInNlbmRlcjAzXFxcIiAocmVhZC1rZXlzZXQgXFxcInNlbmRlcjAzXFxcIikgMTMwMDAwMDAwLjApXFxuKGNvaW4uY29pbmJhc2UgXFxcInNlbmRlcjA0XFxcIiAocmVhZC1rZXlzZXQgXFxcInNlbmRlcjA0XFxcIikgMTQwMDAwMDAwLjApXFxuKGNvaW4uY29pbmJhc2UgXFxcInNlbmRlcjA1XFxcIiAocmVhZC1rZXlzZXQgXFxcInNlbmRlcjA1XFxcIikgMTUwMDAwMDAwLjApXFxuKGNvaW4uY29pbmJhc2UgXFxcInNlbmRlcjA2XFxcIiAocmVhZC1rZXlzZXQgXFxcInNlbmRlcjA2XFxcIikgMTYwMDAwMDAwLjApXFxuKGNvaW4uY29pbmJhc2UgXFxcInNlbmRlcjA3XFxcIiAocmVhZC1rZXlzZXQgXFxcInNlbmRlcjA3XFxcIikgMTcwMDAwMDAwLjApXFxuKGNvaW4uY29pbmJhc2UgXFxcInNlbmRlcjA4XFxcIiAocmVhZC1rZXlzZXQgXFxcInNlbmRlcjA4XFxcIikgMTgwMDAwMDAwLjApXFxuKGNvaW4uY29pbmJhc2UgXFxcInNlbmRlcjA5XFxcIiAocmVhZC1rZXlzZXQgXFxcInNlbmRlcjA5XFxcIikgMTkwMDAwMDAwLjApXFxuKGNvaW4uY29pbmJhc2UgXFxcIm11bHRpLTAwLTAxXFxcIiAocmVhZC1rZXlzZXQgXFxcIm11bHRpLTAwLTAxXFxcIikgMTAxMDAwMDAwLjApXFxuKGNvaW4uY29pbmJhc2UgXFxcIm11bHRpLTAyLTAzLTA0LWFueVxcXCIgKHJlYWQta2V5c2V0IFxcXCJtdWx0aS0wMi0wMy0wNC1hbnlcXFwiKSAxMjM0MDAwMDAuMClcIn19LFwic2lnbmVyc1wiOltdLFwibWV0YVwiOntcImNyZWF0aW9uVGltZVwiOjAsXCJ0dGxcIjoxNzI4MDAsXCJnYXNMaW1pdFwiOjAsXCJjaGFpbklkXCI6XCJcIixcImdhc1ByaWNlXCI6MCxcInNlbmRlclwiOlwiXCJ9LFwibm9uY2VcIjpcIlxcXCJ0ZXN0bmV0LWdyYW50c1xcXCJcIn0ifQ
  - eyJnYXMiOjAsInJlc3VsdCI6eyJzdGF0dXMiOiJzdWNjZXNzIiwiZGF0YSI6IldyaXRlIHN1Y2NlZWRlZCJ9LCJyZXFLZXkiOiJyRzJuWGJwSDVHeWxTTWt6TThnOHNMZVY4S0M4NnlwT1RKWkZnb1BUZEdnIiwibG9ncyI6ImVWamN4QndjUnQ4M25COUM0RzFJVUdBVnVqOHdpd0pvM0tJLUI4b3ZEbVEiLCJtZXRhRGF0YSI6bnVsbCwiY29udGludWF0aW9uIjpudWxsLCJ0eElkIjoxfQ
minerData: eyJhY2NvdW50IjoiTm9NaW5lciIsInByZWRpY2F0ZSI6IjwiLCJwdWJsaWMta2V5cyI6W119
transactionsHash: SS9jyRak3_6x1wR_GhglszGCO9v_1qUZvLPJ1v2GNqk
outputsHash: L4qyURThuVRo3IMl_Sl77AqTo74X-KbNfnkTCYZBzbw
payloadHash: vMJNfbjRMssZQWxElWudtlrSrhDZOirC4iCCgtSSQnU
coinbase: eyJnYXMiOjAsInJlc3VsdCI6eyJzdGF0dXMiOiJzdWNjZXNzIiwiZGF0YSI6Ik5PX0NPSU5CQVNFIn0sInJlcUtleSI6IkRsZFJ3Q2JsUTdMb3F5NndZSm5hb2RIbDMwZDNqM2VILXF0RnpmRXY0NmciLCJsb2dzIjpudWxsLCJtZXRhRGF0YSI6bnVsbCwiY29udGludWF0aW9uIjpudWxsLCJ0eElkIjpudWxsfQ

|]
