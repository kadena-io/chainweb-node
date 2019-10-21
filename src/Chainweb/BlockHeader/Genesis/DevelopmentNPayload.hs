{-# LANGUAGE QuasiQuotes #-}

-- This module is auto-generated. DO NOT EDIT IT MANUALLY.

module Chainweb.BlockHeader.Genesis.DevelopmentNPayload ( payloadBlock ) where

import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (decodeThrow)

import NeatInterpolation (text)

import Chainweb.Payload (PayloadWithOutputs)
import Chainweb.Utils (fromJuste)

payloadBlock :: PayloadWithOutputs
payloadBlock = fromJuste $ decodeThrow $ encodeUtf8 [text|
transactions:
- - eyJoYXNoIjoiZHR0eGNDajZmNnBWS1EzdTdvNWoxUzdzVktQcXpwamFTUktUaWZOUmNQWSIsInNpZ3MiOltdLCJjbWQiOiJ7XCJuZXR3b3JrSWRcIjpudWxsLFwicGF5bG9hZFwiOntcImV4ZWNcIjp7XCJkYXRhXCI6bnVsbCxcImNvZGVcIjpcIihpbnRlcmZhY2UgZnVuZ2libGUtdjFcXG5cXG4gIFxcXCIgU3RhbmRhcmQgZm9yIGZ1bmdpYmxlIGNvaW5zIGFuZCB0b2tlbnMgYXMgc3BlY2lmaWVkIGluIEtJUC0wMDAyLiBcXFwiXFxuXFxuICAgOyAtLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tXFxuICAgOyBTY2hlbWFcXG5cXG4gICAoZGVmc2NoZW1hIGFjY291bnQtZGV0YWlsc1xcbiAgICBAZG9jIFxcXCJTY2hlbWEgZm9yIHJlc3VsdHMgb2YgJ2FjY291bnQnIG9wZXJhdGlvbi5cXFwiXFxuICAgIEBtb2RlbCBbIChpbnZhcmlhbnQgKCE9IFxcXCJcXFwiIHNlbmRlcikpIF1cXG5cXG4gICAgYWNjb3VudDpzdHJpbmdcXG4gICAgYmFsYW5jZTpkZWNpbWFsXFxuICAgIGd1YXJkOmd1YXJkKVxcblxcbiAgIChkZWZzY2hlbWEgdHJhbnNmZXItc2NoZW1hXFxuICAgIEBkb2MgXFxcIlNjaGVtYSBmb3IgVFJBTlNGRVItbWdyLiBGdXR1cmUgcGFjdCB3aWxsIGFsbG93IHNjaGVtYSBpbmZlcnJlbmNlLlxcXCJcXG4gICAgQG1vZGVsXFxuICAgICAgWyAoaW52YXJpYW50ICghPSBcXFwiXFxcIiBzZW5kZXIpKVxcbiAgICAgICAgKGludmFyaWFudCAoIT0gXFxcIlxcXCIgcmVjZWl2ZXIpKVxcbiAgICAgICAgKGludmFyaWFudCAoIT0gc2VuZGVyIHJlY2VpdmVyKSlcXG4gICAgICAgIChpbnZhcmlhbnQgKD4gYW1vdW50IDAuMCkpXFxuICAgICAgXVxcblxcbiAgICBzZW5kZXI6c3RyaW5nXFxuICAgIHJlY2VpdmVyOnN0cmluZ1xcbiAgICBhbW91bnQ6ZGVjaW1hbClcXG5cXG4gICA7IC0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS1cXG4gICA7IENhcHNcXG5cXG4gICAoZGVmY2FwIFRSQU5TRkVSOmJvb2xcXG4gICAgICggc2VuZGVyOnN0cmluZ1xcbiAgICAgICByZWNlaXZlcjpzdHJpbmdcXG4gICAgICAgYW1vdW50OmRlY2ltYWxcXG4gICAgIClcXG4gICAgIEBkb2MgXFxcIiBNYW5hZ2VkIGNhcGFiaWxpdHkgc2VhbGluZyBBTU9VTlQgZm9yIHRyYW5zZmVyIGZyb20gU0VOREVSIHRvIFxcXFxcXG4gICAgICAgICAgXFxcXCBSRUNFSVZFUi4gUGVybWl0cyBhbnkgbnVtYmVyIG9mIHRyYW5zZmVycyB1cCB0byBBTU9VTlQuXFxcIlxcbiAgICAgQG1hbmFnZWQgYW1vdW50IFRSQU5TRkVSLW1nclxcbiAgICAgKVxcblxcbiAgIChkZWZ1biBUUkFOU0ZFUi1tZ3I6ZGVjaW1hbFxcbiAgICAgKCBtYW5hZ2VkOmRlY2ltYWxcXG4gICAgICAgcmVxdWVzdGVkOmRlY2ltYWxcXG4gICAgIClcXG4gICAgIEBkb2MgXFxcIiBNYW5hZ2VzIFRSQU5TRkVSIEFNT1VOVCBsaW5lYXJseSwgXFxcXFxcbiAgICAgICAgICBcXFxcIHN1Y2ggdGhhdCBhIHJlcXVlc3QgZm9yIDEuMCBhbW91bnQgb24gYSAzLjAgXFxcXFxcbiAgICAgICAgICBcXFxcIG1hbmFnZWQgcXVhbnRpdHkgZW1pdHMgdXBkYXRlZCBhbW91bnQgMi4wLlxcXCJcXG4gICAgIClcXG5cXG4gICA7IC0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS1cXG4gICA7IEZ1bmN0aW9uYWxpdHlcXG5cXG4gICAoZGVmdW4gdHJhbnNmZXItY3JlYXRlOnN0cmluZ1xcbiAgICAgKCBzZW5kZXI6c3RyaW5nXFxuICAgICAgIHJlY2VpdmVyOnN0cmluZ1xcbiAgICAgICByZWNlaXZlci1ndWFyZDpndWFyZFxcbiAgICAgICBhbW91bnQ6ZGVjaW1hbFxcbiAgICAgKVxcbiAgICAgQGRvYyBcXFwiIFRyYW5zZmVyIEFNT1VOVCBiZXR3ZWVuIGFjY291bnRzIFNFTkRFUiBhbmQgUkVDRUlWRVIuIFxcXFxcXG4gICAgICAgICAgXFxcXCBGYWlscyBpZiBTRU5ERVIgZG9lcyBub3QgZXhpc3QuIElmIFJFQ0VJVkVSIGV4aXN0cywgZ3VhcmQgXFxcXFxcbiAgICAgICAgICBcXFxcIG11c3QgbWF0Y2ggZXhpc3RpbmcgdmFsdWUuIElmIFJFQ0VJVkVSIGRvZXMgbm90IGV4aXN0LCBcXFxcXFxuICAgICAgICAgIFxcXFwgUkVDRUlWRVIgYWNjb3VudCBpcyBjcmVhdGVkIHVzaW5nIFJFQ0VJVkVSLUdVQVJELiBcXFxcXFxuICAgICAgICAgIFxcXFwgU3ViamVjdCB0byBtYW5hZ2VtZW50IGJ5IFRSQU5TRkVSIGNhcGFiaWxpdHkuXFxcIlxcbiAgICAgQG1vZGVsIFsgKHByb3BlcnR5ICg-IGFtb3VudCAwLjApKVxcbiAgICAgICAgICAgICAgKHByb3BlcnR5ICghPSBzZW5kZXIgXFxcIlxcXCIpKVxcbiAgICAgICAgICAgICAgKHByb3BlcnR5ICghPSByZWNlaXZlciBcXFwiXFxcIikpXFxuICAgICAgICAgICAgICAocHJvcGVydHkgKCE9IHNlbmRlciByZWNlaXZlcikpXFxuICAgICAgICAgICAgXVxcbiAgICAgKVxcblxcbiAgIChkZWZwYWN0IHRyYW5zZmVyLWNyb3NzY2hhaW46c3RyaW5nXFxuICAgICAoIHNlbmRlcjpzdHJpbmdcXG4gICAgICAgcmVjZWl2ZXI6c3RyaW5nXFxuICAgICAgIHJlY2VpdmVyLWd1YXJkOmd1YXJkXFxuICAgICAgIHRhcmdldC1jaGFpbjpzdHJpbmdcXG4gICAgICAgYW1vdW50OmRlY2ltYWxcXG4gICAgIClcXG4gICAgIEBkb2MgXFxcIiAyLXN0ZXAgcGFjdCB0byB0cmFuc2ZlciBBTU9VTlQgZnJvbSBTRU5ERVIgb24gY3VycmVudCBjaGFpbiBcXFxcXFxuICAgICAgICAgIFxcXFwgdG8gUkVDRUlWRVIgb24gVEFSR0VULUNIQUlOIHZpYSBTUFYgcHJvb2YuIFxcXFxcXG4gICAgICAgICAgXFxcXCBUQVJHRVQtQ0hBSU4gbXVzdCBiZSBkaWZmZXJlbnQgdGhhbiBjdXJyZW50IGNoYWluIGlkLiBcXFxcXFxuICAgICAgICAgIFxcXFwgRmlyc3Qgc3RlcCBkZWJpdHMgQU1PVU5UIGNvaW5zIGluIFNFTkRFUiBhY2NvdW50IGFuZCB5aWVsZHMgXFxcXFxcbiAgICAgICAgICBcXFxcIFJFQ0VJVkVSLCBSRUNFSVZFUl9HVUFSRCBhbmQgQU1PVU5UIHRvIFRBUkdFVC1DSEFJTi4gXFxcXFxcbiAgICAgICAgICBcXFxcIFNlY29uZCBzdGVwIGNvbnRpbnVhdGlvbiBpcyBzZW50IGludG8gVEFSR0VULUNIQUlOIHdpdGggcHJvb2YgXFxcXFxcbiAgICAgICAgICBcXFxcIG9idGFpbmVkIGZyb20gdGhlIHNwdiAnb3V0cHV0JyBlbmRwb2ludCBvZiBDaGFpbndlYi4gXFxcXFxcbiAgICAgICAgICBcXFxcIFByb29mIGlzIHZhbGlkYXRlZCBhbmQgUkVDRUlWRVIgaXMgY3JlZGl0ZWQgd2l0aCBBTU9VTlQgXFxcXFxcbiAgICAgICAgICBcXFxcIGNyZWF0aW5nIGFjY291bnQgd2l0aCBSRUNFSVZFUl9HVUFSRCBhcyBuZWNlc3NhcnkuXFxcIlxcbiAgICAgQG1vZGVsIFsgKHByb3BlcnR5ICg-IGFtb3VudCAwLjApKVxcbiAgICAgICAgICAgICAgKHByb3BlcnR5ICghPSBzZW5kZXIgXFxcIlxcXCIpKVxcbiAgICAgICAgICAgICAgKHByb3BlcnR5ICghPSByZWNlaXZlciBcXFwiXFxcIikpXFxuICAgICAgICAgICAgICAocHJvcGVydHkgKCE9IHNlbmRlciByZWNlaXZlcikpXFxuICAgICAgICAgICAgICAocHJvcGVydHkgKCE9IHRhcmdldC1jaGFpbiBcXFwiXFxcIikpXFxuICAgICAgICAgICAgXVxcbiAgICAgKVxcblxcbiAgIChkZWZ1biBnZXQtYmFsYW5jZTpkZWNpbWFsXFxuICAgICAoIGFjY291bnQ6c3RyaW5nIClcXG4gICAgIFxcXCIgR2V0IGJhbGFuY2UgZm9yIEFDQ09VTlQuIEZhaWxzIGlmIGFjY291bnQgZG9lcyBub3QgZXhpc3QuXFxcIlxcbiAgICAgKVxcblxcbiAgIChkZWZ1biBkZXRhaWxzOm9iamVjdHthY2NvdW50LWRldGFpbHN9XFxuICAgICAoIGFjY291bnQ6IHN0cmluZyApXFxuICAgICBcXFwiIEdldCBhbiBvYmplY3Qgd2l0aCBkZXRhaWxzIG9mIEFDQ09VTlQuIFxcXFxcXG4gICAgIFxcXFwgRmFpbHMgaWYgYWNjb3VudCBkb2VzIG5vdCBleGlzdC5cXFwiXFxuICAgICApXFxuXFxuICAgKGRlZnVuIHByZWNpc2lvbjppbnRlZ2VyXFxuICAgICAoKVxcbiAgICAgXFxcIlJldHVybiB0aGUgbWF4aW11bSBhbGxvd2VkIGRlY2ltYWwgcHJlY2lzaW9uLlxcXCJcXG4gICAgIClcXG5cXG4gICAoZGVmdW4gZW5mb3JjZS11bml0OmJvb2xcXG4gICAgICggYW1vdW50OmRlY2ltYWwgKVxcbiAgICAgXFxcIiBFbmZvcmNlIG1pbmltdW0gcHJlY2lzaW9uIGFsbG93ZWQgZm9yIHRyYW5zYWN0aW9ucy5cXFwiXFxuICAgICApXFxuXFxuICAgKGRlZnVuIGNyZWF0ZS1hY2NvdW50OnN0cmluZ1xcbiAgICAgKCBhY2NvdW50OnN0cmluZ1xcbiAgICAgICBndWFyZDpndWFyZFxcbiAgICAgKVxcbiAgICAgXFxcIiBDcmVhdGUgQUNDT1VOVCB3aXRoIDAuMCBiYWxhbmNlLCB3aXRoIEdVQVJEIGNvbnRyb2xsaW5nIGFjY2Vzcy5cXFwiXFxuICAgICApXFxuXFxuICAgKGRlZnVuIHJvdGF0ZTpzdHJpbmdcXG4gICAgICggYWNjb3VudDpzdHJpbmdcXG4gICAgICAgbmV3LWd1YXJkOmd1YXJkXFxuICAgICApXFxuICAgICBcXFwiIFJvdGF0ZSBndWFyZCBmb3IgQUNDT1VOVC4gVHJhbnNhY3Rpb24gaXMgdmFsaWRhdGVkIGFnYWluc3QgXFxcXFxcbiAgICAgXFxcXCBleGlzdGluZyBndWFyZCBiZWZvcmUgaW5zdGFsbGluZyBuZXcgZ3VhcmQuIFxcXCJcXG4gICAgIClcXG5cXG4pXFxuXCJ9fSxcInNpZ25lcnNcIjpbXSxcIm1ldGFcIjp7XCJjcmVhdGlvblRpbWVcIjowLFwidHRsXCI6MTcyODAwLFwiZ2FzTGltaXRcIjowLFwiY2hhaW5JZFwiOlwiXCIsXCJnYXNQcmljZVwiOjAsXCJzZW5kZXJcIjpcIlwifSxcIm5vbmNlXCI6XCJcXFwiZ2VuZXNpcy0wMVxcXCJcIn0ifQ
  - eyJnYXMiOjAsInJlc3VsdCI6eyJzdGF0dXMiOiJzdWNjZXNzIiwiZGF0YSI6IkxvYWRlZCBpbnRlcmZhY2UgZnVuZ2libGUtdjEifSwicmVxS2V5IjoiZHR0eGNDajZmNnBWS1EzdTdvNWoxUzdzVktQcXpwamFTUktUaWZOUmNQWSIsImxvZ3MiOiJ0VVl4YUtjbjRlWVRLRC1OMDZheG1qZ0dQU294bXhIZVpmNlhYQlFZclRZIiwibWV0YURhdGEiOm51bGwsImNvbnRpbnVhdGlvbiI6bnVsbCwidHhJZCI6MH0
- - eyJoYXNoIjoiNGZVZnFTV1M3R3U5RjM1VjdPMVd3TWcxdW0tbjZ6amZLaVRYYTllTzR6MCIsInNpZ3MiOltdLCJjbWQiOiJ7XCJuZXR3b3JrSWRcIjpudWxsLFwicGF5bG9hZFwiOntcImV4ZWNcIjp7XCJkYXRhXCI6bnVsbCxcImNvZGVcIjpcIihtb2R1bGUgY29pbiBHT1ZFUk5BTkNFXFxuXFxuICBAZG9jIFxcXCInY29pbicgcmVwcmVzZW50cyB0aGUgS2FkZW5hIENvaW4gQ29udHJhY3QuIFRoaXMgY29udHJhY3QgcHJvdmlkZXMgYm90aCB0aGUgXFxcXFxcbiAgXFxcXGJ1eS9yZWRlZW0gZ2FzIHN1cHBvcnQgaW4gdGhlIGZvcm0gb2YgJ2Z1bmQtdHgnLCBhcyB3ZWxsIGFzIHRyYW5zZmVyLCAgICAgICBcXFxcXFxuICBcXFxcY3JlZGl0LCBkZWJpdCwgY29pbmJhc2UsIGFjY291bnQgY3JlYXRpb24gYW5kIHF1ZXJ5LCBhcyB3ZWxsIGFzIFNQViBidXJuICAgIFxcXFxcXG4gIFxcXFxjcmVhdGUuIFRvIGFjY2VzcyB0aGUgY29pbiBjb250cmFjdCwgeW91IG1heSB1c2UgaXRzIGZ1bGx5LXF1YWxpZmllZCBuYW1lLCAgXFxcXFxcbiAgXFxcXG9yIGlzc3VlIHRoZSAnKHVzZSBjb2luKScgY29tbWFuZCBpbiB0aGUgYm9keSBvZiBhIG1vZHVsZSBkZWNsYXJhdGlvbi5cXFwiXFxuXFxuICBAbW9kZWxcXG4gICAgWyAoZGVmcHJvcGVydHkgY29uc2VydmVzLW1hc3NcXG4gICAgICAgICg9IChjb2x1bW4tZGVsdGEgY29pbi10YWJsZSAnYmFsYW5jZSkgMC4wKSlcXG5cXG4gICAgICAoZGVmcHJvcGVydHkgdmFsaWQtYWNjb3VudCAoYWNjb3VudDpzdHJpbmcpXFxuICAgICAgICAoYW5kXFxuICAgICAgICAgICg-PSAobGVuZ3RoIGFjY291bnQpIDMpXFxuICAgICAgICAgICg8PSAobGVuZ3RoIGFjY291bnQpIDI1NikpKVxcbiAgICBdXFxuXFxuICAoaW1wbGVtZW50cyBmdW5naWJsZS12MSlcXG5cXG4gIDsgLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS1cXG4gIDsgU2NoZW1hcyBhbmQgVGFibGVzXFxuXFxuICAoZGVmc2NoZW1hIGNvaW4tc2NoZW1hXFxuICAgIEBkb2MgXFxcIlRoZSBjb2luIGNvbnRyYWN0IHRva2VuIHNjaGVtYVxcXCJcXG4gICAgQG1vZGVsIFsgKGludmFyaWFudCAoPj0gYmFsYW5jZSAwLjApKSBdXFxuXFxuICAgIGJhbGFuY2U6ZGVjaW1hbFxcbiAgICBndWFyZDpndWFyZClcXG5cXG4gIChkZWZ0YWJsZSBjb2luLXRhYmxlOntjb2luLXNjaGVtYX0pXFxuXFxuICA7IC0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tXFxuICA7IENhcGFiaWxpdGllc1xcblxcbiAgKGRlZmNhcCBHT1ZFUk5BTkNFICgpXFxuICAgIChlbmZvcmNlIGZhbHNlIFxcXCJFbmZvcmNlIG5vbi11cGdyYWRlYWJpbGl0eVxcXCIpKVxcblxcbiAgKGRlZmNhcCBHQVMgKClcXG4gICAgXFxcIk1hZ2ljIGNhcGFiaWxpdHkgdG8gcHJvdGVjdCBnYXMgYnV5IGFuZCByZWRlZW1cXFwiXFxuICAgIHRydWUpXFxuXFxuICAoZGVmY2FwIENPSU5CQVNFICgpXFxuICAgIFxcXCJNYWdpYyBjYXBhYmlsaXR5IHRvIHByb3RlY3QgbWluZXIgcmV3YXJkXFxcIlxcbiAgICB0cnVlKVxcblxcbiAgKGRlZmNhcCBHRU5FU0lTICgpXFxuICAgIFxcXCJNYWdpYyBjYXBhYmlsaXR5IGNvbnN0cmFpbmluZyBnZW5lc2lzIHRyYW5zYWN0aW9uc1xcXCJcXG4gICAgdHJ1ZSlcXG5cXG4gIChkZWZjYXAgREVCSVQgKHNlbmRlcjpzdHJpbmcpXFxuICAgIFxcXCJDYXBhYmlsaXR5IGZvciBtYW5hZ2luZyBkZWJpdGluZyBvcGVyYXRpb25zXFxcIlxcbiAgICAoZW5mb3JjZS1ndWFyZCAoYXQgJ2d1YXJkIChyZWFkIGNvaW4tdGFibGUgc2VuZGVyKSkpXFxuICAgIChlbmZvcmNlICghPSBzZW5kZXIgXFxcIlxcXCIpIFxcXCJ2YWxpZCBzZW5kZXJcXFwiKSlcXG5cXG4gIChkZWZjYXAgQ1JFRElUIChyZWNlaXZlcjpzdHJpbmcpXFxuICAgIFxcXCJDYXBhYmlsaXR5IGZvciBtYW5hZ2luZyBjcmVkaXRpbmcgb3BlcmF0aW9uc1xcXCJcXG4gICAgKGVuZm9yY2UgKCE9IHJlY2VpdmVyIFxcXCJcXFwiKSBcXFwidmFsaWQgcmVjZWl2ZXJcXFwiKSlcXG5cXG4gIChkZWZjYXAgVFJBTlNGRVI6Ym9vbFxcbiAgICAoIHNlbmRlcjpzdHJpbmdcXG4gICAgICByZWNlaXZlcjpzdHJpbmdcXG4gICAgICBhbW91bnQ6ZGVjaW1hbFxcbiAgICApXFxuICAgIEBtYW5hZ2VkIGFtb3VudCBUUkFOU0ZFUi1tZ3JcXG4gICAgKGVuZm9yY2UgKCE9IHNlbmRlciByZWNlaXZlcikgXFxcInNhbWUgc2VuZGVyIGFuZCByZWNlaXZlclxcXCIpXFxuICAgIChlbmZvcmNlLXVuaXQgYW1vdW50KVxcbiAgICAoZW5mb3JjZSAoPiBhbW91bnQgMC4wKSBcXFwiUG9zaXRpdmUgYW1vdW50XFxcIilcXG4gICAgKGNvbXBvc2UtY2FwYWJpbGl0eSAoREVCSVQgc2VuZGVyKSlcXG4gICAgKGNvbXBvc2UtY2FwYWJpbGl0eSAoQ1JFRElUIHJlY2VpdmVyKSlcXG4gIClcXG5cXG4gIChkZWZ1biBUUkFOU0ZFUi1tZ3I6ZGVjaW1hbFxcbiAgICAoIG1hbmFnZWQ6ZGVjaW1hbFxcbiAgICAgIHJlcXVlc3RlZDpkZWNpbWFsXFxuICAgIClcXG5cXG4gICAgKGxldCAoKG5ld2JhbCAoLSBtYW5hZ2VkIHJlcXVlc3RlZCkpKVxcbiAgICAgIChlbmZvcmNlICg-PSBuZXdiYWwgMC4wKVxcbiAgICAgICAgKGZvcm1hdCBcXFwiVFJBTlNGRVIgZXhjZWVkZWQgZm9yIGJhbGFuY2Uge31cXFwiIFttYW5hZ2VkXSkpXFxuICAgICAgbmV3YmFsKVxcbiAgKVxcblxcbiAgOyAtLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLVxcbiAgOyBDb25zdGFudHNcXG5cXG4gIChkZWZjb25zdCBDT0lOX0NIQVJTRVQgQ0hBUlNFVF9MQVRJTjFcXG4gICAgXFxcIlRoZSBkZWZhdWx0IGNvaW4gY29udHJhY3QgY2hhcmFjdGVyIHNldFxcXCIpXFxuXFxuICAoZGVmY29uc3QgTUlOSU1VTV9QUkVDSVNJT04gMTJcXG4gICAgXFxcIk1pbmltdW0gYWxsb3dlZCBwcmVjaXNpb24gZm9yIGNvaW4gdHJhbnNhY3Rpb25zXFxcIilcXG5cXG4gIChkZWZjb25zdCBNSU5JTVVNX0FDQ09VTlRfTEVOR1RIIDNcXG4gICAgXFxcIk1pbmltdW0gYWNjb3VudCBsZW5ndGggYWRtaXNzaWJsZSBmb3IgY29pbiBhY2NvdW50c1xcXCIpXFxuXFxuICAoZGVmY29uc3QgTUFYSU1VTV9BQ0NPVU5UX0xFTkdUSCAyNTZcXG4gICAgXFxcIk1heGltdW0gYWNjb3VudCBuYW1lIGxlbmd0aCBhZG1pc3NpYmxlIGZvciBjb2luIGFjY291bnRzXFxcIilcXG5cXG4gIDsgLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS1cXG4gIDsgVXRpbGl0aWVzXFxuXFxuICAoZGVmdW4gZW5mb3JjZS11bml0OmJvb2wgKGFtb3VudDpkZWNpbWFsKVxcbiAgICBAZG9jIFxcXCJFbmZvcmNlIG1pbmltdW0gcHJlY2lzaW9uIGFsbG93ZWQgZm9yIGNvaW4gdHJhbnNhY3Rpb25zXFxcIlxcblxcbiAgICAoZW5mb3JjZVxcbiAgICAgICg9IChmbG9vciBhbW91bnQgTUlOSU1VTV9QUkVDSVNJT04pXFxuICAgICAgICAgYW1vdW50KVxcbiAgICAgIChmb3JtYXQgXFxcIkFtb3VudCB2aW9sYXRlcyBtaW5pbXVtIHByZWNpc2lvbjoge31cXFwiIFthbW91bnRdKSlcXG4gICAgKVxcblxcbiAgKGRlZnVuIHZhbGlkYXRlLWFjY291bnQgKGFjY291bnQ6c3RyaW5nKVxcbiAgICBAZG9jIFxcXCJFbmZvcmNlIHRoYXQgYW4gYWNjb3VudCBuYW1lIGNvbmZvcm1zIHRvIHRoZSBjb2luIGNvbnRyYWN0IFxcXFxcXG4gICAgICAgICBcXFxcbWluaW11bSBhbmQgbWF4aW11bSBsZW5ndGggcmVxdWlyZW1lbnRzLCBhcyB3ZWxsIGFzIHRoZSAgICBcXFxcXFxuICAgICAgICAgXFxcXGxhdGluLTEgY2hhcmFjdGVyIHNldC5cXFwiXFxuXFxuICAgIChlbmZvcmNlXFxuICAgICAgKGlzLWNoYXJzZXQgQ09JTl9DSEFSU0VUIGFjY291bnQpXFxuICAgICAgKGZvcm1hdFxcbiAgICAgICAgXFxcIkFjY291bnQgZG9lcyBub3QgY29uZm9ybSB0byB0aGUgY29pbiBjb250cmFjdCBjaGFyc2V0OiB7fVxcXCJcXG4gICAgICAgIFthY2NvdW50XSkpXFxuXFxuICAgIChsZXQgKChhY2NvdW50LWxlbmd0aCAobGVuZ3RoIGFjY291bnQpKSlcXG5cXG4gICAgICAoZW5mb3JjZVxcbiAgICAgICAgKD49IGFjY291bnQtbGVuZ3RoIE1JTklNVU1fQUNDT1VOVF9MRU5HVEgpXFxuICAgICAgICAoZm9ybWF0XFxuICAgICAgICAgIFxcXCJBY2NvdW50IG5hbWUgZG9lcyBub3QgY29uZm9ybSB0byB0aGUgbWluIGxlbmd0aCByZXF1aXJlbWVudDoge31cXFwiXFxuICAgICAgICAgIFthY2NvdW50XSkpXFxuXFxuICAgICAgKGVuZm9yY2VcXG4gICAgICAgICg8PSBhY2NvdW50LWxlbmd0aCBNQVhJTVVNX0FDQ09VTlRfTEVOR1RIKVxcbiAgICAgICAgKGZvcm1hdFxcbiAgICAgICAgICBcXFwiQWNjb3VudCBuYW1lIGRvZXMgbm90IGNvbmZvcm0gdG8gdGhlIG1pbiBsZW5ndGggcmVxdWlyZW1lbnQ6IHt9XFxcIlxcbiAgICAgICAgICBbYWNjb3VudF0pKVxcbiAgICAgIClcXG4gIClcXG5cXG4gIDsgLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS1cXG4gIDsgQ29pbiBDb250cmFjdFxcblxcbiAgKGRlZnVuIGJ1eS1nYXM6c3RyaW5nIChzZW5kZXI6c3RyaW5nIHRvdGFsOmRlY2ltYWwpXFxuICAgIEBkb2MgXFxcIlRoaXMgZnVuY3Rpb24gZGVzY3JpYmVzIHRoZSBtYWluICdnYXMgYnV5JyBvcGVyYXRpb24uIEF0IHRoaXMgcG9pbnQgXFxcXFxcbiAgICBcXFxcTUlORVIgaGFzIGJlZW4gY2hvc2VuIGZyb20gdGhlIHBvb2wsIGFuZCB3aWxsIGJlIHZhbGlkYXRlZC4gVGhlIFNFTkRFUiAgIFxcXFxcXG4gICAgXFxcXG9mIHRoaXMgdHJhbnNhY3Rpb24gaGFzIHNwZWNpZmllZCBhIGdhcyBsaW1pdCBMSU1JVCAobWF4aW11bSBnYXMpIGZvciAgICBcXFxcXFxuICAgIFxcXFx0aGUgdHJhbnNhY3Rpb24sIGFuZCB0aGUgcHJpY2UgaXMgdGhlIHNwb3QgcHJpY2Ugb2YgZ2FzIGF0IHRoYXQgdGltZS4gICAgXFxcXFxcbiAgICBcXFxcVGhlIGdhcyBidXkgd2lsbCBiZSBleGVjdXRlZCBwcmlvciB0byBleGVjdXRpbmcgU0VOREVSJ3MgY29kZS5cXFwiXFxuXFxuICAgIEBtb2RlbCBbIChwcm9wZXJ0eSAoPiB0b3RhbCAwLjApKVxcbiAgICAgICAgICAgICAocHJvcGVydHkgKHZhbGlkLWFjY291bnQgc2VuZGVyKSlcXG4gICAgICAgICAgIF1cXG5cXG4gICAgKHZhbGlkYXRlLWFjY291bnQgc2VuZGVyKVxcblxcbiAgICAoZW5mb3JjZS11bml0IHRvdGFsKVxcbiAgICAoZW5mb3JjZSAoPiB0b3RhbCAwLjApIFxcXCJnYXMgc3VwcGx5IG11c3QgYmUgYSBwb3NpdGl2ZSBxdWFudGl0eVxcXCIpXFxuXFxuICAgIChyZXF1aXJlLWNhcGFiaWxpdHkgKEdBUykpXFxuICAgICh3aXRoLWNhcGFiaWxpdHkgKERFQklUIHNlbmRlcilcXG4gICAgICAoZGViaXQgc2VuZGVyIHRvdGFsKSlcXG4gICAgKVxcblxcbiAgKGRlZnVuIHJlZGVlbS1nYXM6c3RyaW5nIChtaW5lcjpzdHJpbmcgbWluZXItZ3VhcmQ6Z3VhcmQgc2VuZGVyOnN0cmluZyB0b3RhbDpkZWNpbWFsKVxcbiAgICBAZG9jIFxcXCJUaGlzIGZ1bmN0aW9uIGRlc2NyaWJlcyB0aGUgbWFpbiAncmVkZWVtIGdhcycgb3BlcmF0aW9uLiBBdCB0aGlzICAgIFxcXFxcXG4gICAgXFxcXHBvaW50LCB0aGUgU0VOREVSJ3MgdHJhbnNhY3Rpb24gaGFzIGJlZW4gZXhlY3V0ZWQsIGFuZCB0aGUgZ2FzIHRoYXQgICAgICBcXFxcXFxuICAgIFxcXFx3YXMgY2hhcmdlZCBoYXMgYmVlbiBjYWxjdWxhdGVkLiBNSU5FUiB3aWxsIGJlIGNyZWRpdGVkIHRoZSBnYXMgY29zdCwgICAgXFxcXFxcbiAgICBcXFxcYW5kIFNFTkRFUiB3aWxsIHJlY2VpdmUgdGhlIHJlbWFpbmRlciB1cCB0byB0aGUgbGltaXRcXFwiXFxuXFxuICAgIEBtb2RlbCBbIChwcm9wZXJ0eSAoPiB0b3RhbCAwLjApKVxcbiAgICAgICAgICAgICAocHJvcGVydHkgKHZhbGlkLWFjY291bnQgc2VuZGVyKSlcXG4gICAgICAgICAgICAgKHByb3BlcnR5ICh2YWxpZC1hY2NvdW50IG1pbmVyKSlcXG4gICAgICAgICAgIF1cXG5cXG4gICAgKHZhbGlkYXRlLWFjY291bnQgc2VuZGVyKVxcbiAgICAodmFsaWRhdGUtYWNjb3VudCBtaW5lcilcXG4gICAgKGVuZm9yY2UtdW5pdCB0b3RhbClcXG5cXG4gICAgKHJlcXVpcmUtY2FwYWJpbGl0eSAoR0FTKSlcXG4gICAgKGxldCpcXG4gICAgICAoKGZlZSAocmVhZC1kZWNpbWFsIFxcXCJmZWVcXFwiKSlcXG4gICAgICAgKHJlZnVuZCAoLSB0b3RhbCBmZWUpKSlcXG5cXG4gICAgICAoZW5mb3JjZS11bml0IGZlZSlcXG4gICAgICAoZW5mb3JjZSAoPj0gZmVlIDAuMClcXG4gICAgICAgIFxcXCJmZWUgbXVzdCBiZSBhIG5vbi1uZWdhdGl2ZSBxdWFudGl0eVxcXCIpXFxuXFxuICAgICAgKGVuZm9yY2UgKD49IHJlZnVuZCAwLjApXFxuICAgICAgICBcXFwicmVmdW5kIG11c3QgYmUgYSBub24tbmVnYXRpdmUgcXVhbnRpdHlcXFwiKVxcblxcbiAgICAgICAgOyBkaXJlY3RseSB1cGRhdGUgaW5zdGVhZCBvZiBjcmVkaXRcXG4gICAgICAod2l0aC1jYXBhYmlsaXR5IChDUkVESVQgc2VuZGVyKVxcbiAgICAgICAgKGlmICg-IHJlZnVuZCAwLjApXFxuICAgICAgICAgICh3aXRoLXJlYWQgY29pbi10YWJsZSBzZW5kZXJcXG4gICAgICAgICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6PSBiYWxhbmNlIH1cXG4gICAgICAgICAgICAodXBkYXRlIGNvaW4tdGFibGUgc2VuZGVyXFxuICAgICAgICAgICAgICB7IFxcXCJiYWxhbmNlXFxcIjogKCsgYmFsYW5jZSByZWZ1bmQpIH0pKVxcblxcbiAgICAgICAgICBcXFwibm9vcFxcXCIpKVxcblxcbiAgICAgICh3aXRoLWNhcGFiaWxpdHkgKENSRURJVCBtaW5lcilcXG4gICAgICAgIChpZiAoPiBmZWUgMC4wKVxcbiAgICAgICAgICAoY3JlZGl0IG1pbmVyIG1pbmVyLWd1YXJkIGZlZSlcXG4gICAgICAgICAgXFxcIm5vb3BcXFwiKSlcXG4gICAgICApXFxuXFxuICAgIClcXG5cXG4gIChkZWZ1biBjcmVhdGUtYWNjb3VudDpzdHJpbmcgKGFjY291bnQ6c3RyaW5nIGd1YXJkOmd1YXJkKVxcbiAgICBAbW9kZWwgWyAocHJvcGVydHkgKHZhbGlkLWFjY291bnQgYWNjb3VudCkpIF1cXG5cXG4gICAgKHZhbGlkYXRlLWFjY291bnQgYWNjb3VudClcXG5cXG4gICAgKGluc2VydCBjb2luLXRhYmxlIGFjY291bnRcXG4gICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6IDAuMFxcbiAgICAgICwgXFxcImd1YXJkXFxcIiAgIDogZ3VhcmRcXG4gICAgICB9KVxcbiAgICApXFxuXFxuICAoZGVmdW4gZ2V0LWJhbGFuY2U6ZGVjaW1hbCAoYWNjb3VudDpzdHJpbmcpXFxuICAgICh3aXRoLXJlYWQgY29pbi10YWJsZSBhY2NvdW50XFxuICAgICAgeyBcXFwiYmFsYW5jZVxcXCIgOj0gYmFsYW5jZSB9XFxuICAgICAgYmFsYW5jZVxcbiAgICAgIClcXG4gICAgKVxcblxcbiAgKGRlZnVuIGRldGFpbHM6b2JqZWN0e2Z1bmdpYmxlLXYxLmFjY291bnQtZGV0YWlsc31cXG4gICAgKCBhY2NvdW50OnN0cmluZyApXFxuICAgICh3aXRoLXJlYWQgY29pbi10YWJsZSBhY2NvdW50XFxuICAgICAgeyBcXFwiYmFsYW5jZVxcXCIgOj0gYmFsXFxuICAgICAgLCBcXFwiZ3VhcmRcXFwiIDo9IGcgfVxcbiAgICAgIHsgXFxcImFjY291bnRcXFwiIDogYWNjb3VudFxcbiAgICAgICwgXFxcImJhbGFuY2VcXFwiIDogYmFsXFxuICAgICAgLCBcXFwiZ3VhcmRcXFwiOiBnIH0pXFxuICAgIClcXG5cXG4gIChkZWZ1biByb3RhdGU6c3RyaW5nIChhY2NvdW50OnN0cmluZyBuZXctZ3VhcmQ6Z3VhcmQpXFxuXFxuICAgICh3aXRoLXJlYWQgY29pbi10YWJsZSBhY2NvdW50XFxuICAgICAgeyBcXFwiZ3VhcmRcXFwiIDo9IG9sZC1ndWFyZCB9XFxuXFxuICAgICAgKGVuZm9yY2UtZ3VhcmQgb2xkLWd1YXJkKVxcbiAgICAgIChlbmZvcmNlLWd1YXJkIG5ldy1ndWFyZClcXG5cXG4gICAgICAodXBkYXRlIGNvaW4tdGFibGUgYWNjb3VudFxcbiAgICAgICAgeyBcXFwiZ3VhcmRcXFwiIDogbmV3LWd1YXJkIH1cXG4gICAgICAgICkpKVxcblxcblxcbiAgKGRlZnVuIHByZWNpc2lvbjppbnRlZ2VyXFxuICAgICgpXFxuICAgIE1JTklNVU1fUFJFQ0lTSU9OKVxcblxcbiAgKGRlZnVuIHRyYW5zZmVyOnN0cmluZyAoc2VuZGVyOnN0cmluZyByZWNlaXZlcjpzdHJpbmcgYW1vdW50OmRlY2ltYWwpXFxuICAgIEBtb2RlbCBbIChwcm9wZXJ0eSBjb25zZXJ2ZXMtbWFzcylcXG4gICAgICAgICAgICAgKHByb3BlcnR5ICg-IGFtb3VudCAwLjApKVxcbiAgICAgICAgICAgICAocHJvcGVydHkgKHZhbGlkLWFjY291bnQgc2VuZGVyKSlcXG4gICAgICAgICAgICAgKHByb3BlcnR5ICh2YWxpZC1hY2NvdW50IHJlY2VpdmVyKSlcXG4gICAgICAgICAgICAgKHByb3BlcnR5ICghPSBzZW5kZXIgcmVjZWl2ZXIpKSBdXFxuXFxuICAgIChlbmZvcmNlICghPSBzZW5kZXIgcmVjZWl2ZXIpXFxuICAgICAgXFxcInNlbmRlciBjYW5ub3QgYmUgdGhlIHJlY2VpdmVyIG9mIGEgdHJhbnNmZXJcXFwiKVxcblxcbiAgICAodmFsaWRhdGUtYWNjb3VudCBzZW5kZXIpXFxuICAgICh2YWxpZGF0ZS1hY2NvdW50IHJlY2VpdmVyKVxcblxcbiAgICAoZW5mb3JjZSAoPiBhbW91bnQgMC4wKVxcbiAgICAgIFxcXCJ0cmFuc2ZlciBhbW91bnQgbXVzdCBiZSBwb3NpdGl2ZVxcXCIpXFxuXFxuICAgIChlbmZvcmNlLXVuaXQgYW1vdW50KVxcblxcbiAgICAod2l0aC1jYXBhYmlsaXR5IChUUkFOU0ZFUiBzZW5kZXIgcmVjZWl2ZXIgYW1vdW50KVxcbiAgICAgIChkZWJpdCBzZW5kZXIgYW1vdW50KVxcbiAgICAgICh3aXRoLXJlYWQgY29pbi10YWJsZSByZWNlaXZlclxcbiAgICAgICAgeyBcXFwiZ3VhcmRcXFwiIDo9IGcgfVxcblxcbiAgICAgICAgKGNyZWRpdCByZWNlaXZlciBnIGFtb3VudCkpXFxuICAgICAgKVxcbiAgICApXFxuXFxuICAoZGVmdW4gdHJhbnNmZXItY3JlYXRlOnN0cmluZ1xcbiAgICAoIHNlbmRlcjpzdHJpbmdcXG4gICAgICByZWNlaXZlcjpzdHJpbmdcXG4gICAgICByZWNlaXZlci1ndWFyZDpndWFyZFxcbiAgICAgIGFtb3VudDpkZWNpbWFsIClcXG5cXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5IGNvbnNlcnZlcy1tYXNzKSBdXFxuXFxuICAgIChlbmZvcmNlICghPSBzZW5kZXIgcmVjZWl2ZXIpXFxuICAgICAgXFxcInNlbmRlciBjYW5ub3QgYmUgdGhlIHJlY2VpdmVyIG9mIGEgdHJhbnNmZXJcXFwiKVxcblxcbiAgICAodmFsaWRhdGUtYWNjb3VudCBzZW5kZXIpXFxuICAgICh2YWxpZGF0ZS1hY2NvdW50IHJlY2VpdmVyKVxcblxcbiAgICAoZW5mb3JjZSAoPiBhbW91bnQgMC4wKVxcbiAgICAgIFxcXCJ0cmFuc2ZlciBhbW91bnQgbXVzdCBiZSBwb3NpdGl2ZVxcXCIpXFxuXFxuICAgIChlbmZvcmNlLXVuaXQgYW1vdW50KVxcblxcbiAgICAod2l0aC1jYXBhYmlsaXR5IChUUkFOU0ZFUiBzZW5kZXIgcmVjZWl2ZXIgYW1vdW50KVxcbiAgICAgIChkZWJpdCBzZW5kZXIgYW1vdW50KVxcbiAgICAgIChjcmVkaXQgcmVjZWl2ZXIgcmVjZWl2ZXItZ3VhcmQgYW1vdW50KSlcXG4gICAgKVxcblxcbiAgKGRlZnVuIGNvaW5iYXNlOnN0cmluZyAoYWNjb3VudDpzdHJpbmcgYWNjb3VudC1ndWFyZDpndWFyZCBhbW91bnQ6ZGVjaW1hbClcXG4gICAgQGRvYyBcXFwiSW50ZXJuYWwgZnVuY3Rpb24gZm9yIHRoZSBpbml0aWFsIGNyZWF0aW9uIG9mIGNvaW5zLiAgVGhpcyBmdW5jdGlvbiBcXFxcXFxuICAgIFxcXFxjYW5ub3QgYmUgdXNlZCBvdXRzaWRlIG9mIHRoZSBjb2luIGNvbnRyYWN0LlxcXCJcXG5cXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5ICh2YWxpZC1hY2NvdW50IGFjY291bnQpKSBdXFxuXFxuICAgICh2YWxpZGF0ZS1hY2NvdW50IGFjY291bnQpXFxuICAgIChlbmZvcmNlLXVuaXQgYW1vdW50KVxcblxcbiAgICAocmVxdWlyZS1jYXBhYmlsaXR5IChDT0lOQkFTRSkpXFxuICAgICh3aXRoLWNhcGFiaWxpdHkgKENSRURJVCBhY2NvdW50KVxcbiAgICAgIChjcmVkaXQgYWNjb3VudCBhY2NvdW50LWd1YXJkIGFtb3VudCkpXFxuICAgIClcXG5cXG4gIChkZWZwYWN0IGZ1bmQtdHggKHNlbmRlcjpzdHJpbmcgbWluZXI6c3RyaW5nIG1pbmVyLWd1YXJkOmd1YXJkIHRvdGFsOmRlY2ltYWwpXFxuICAgIEBkb2MgXFxcIidmdW5kLXR4JyBpcyBhIHNwZWNpYWwgcGFjdCB0byBmdW5kIGEgdHJhbnNhY3Rpb24gaW4gdHdvIHN0ZXBzLCAgICAgXFxcXFxcbiAgICBcXFxcd2l0aCB0aGUgYWN0dWFsIHRyYW5zYWN0aW9uIHRyYW5zcGlyaW5nIGluIHRoZSBtaWRkbGU6ICAgICAgICAgICAgICAgICAgIFxcXFxcXG4gICAgXFxcXCAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBcXFxcXFxuICAgIFxcXFwgIDEpIEEgYnV5aW5nIHBoYXNlLCBkZWJpdGluZyB0aGUgc2VuZGVyIGZvciB0b3RhbCBnYXMgYW5kIGZlZSwgeWllbGRpbmcgXFxcXFxcbiAgICBcXFxcICAgICBUWF9NQVhfQ0hBUkdFLiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFxcXFxcXG4gICAgXFxcXCAgMikgQSBzZXR0bGVtZW50IHBoYXNlLCByZXN1bWluZyBUWF9NQVhfQ0hBUkdFLCBhbmQgYWxsb2NhdGluZyB0byB0aGUgICBcXFxcXFxuICAgIFxcXFwgICAgIGNvaW5iYXNlIGFjY291bnQgZm9yIHVzZWQgZ2FzIGFuZCBmZWUsIGFuZCBzZW5kZXIgYWNjb3VudCBmb3IgYmFsLSAgXFxcXFxcbiAgICBcXFxcICAgICBhbmNlICh1bnVzZWQgZ2FzLCBpZiBhbnkpLlxcXCJcXG5cXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5ICg-IHRvdGFsIDAuMCkpXFxuICAgICAgICAgICAgIChwcm9wZXJ0eSAodmFsaWQtYWNjb3VudCBzZW5kZXIpKVxcbiAgICAgICAgICAgICAocHJvcGVydHkgKHZhbGlkLWFjY291bnQgbWluZXIpKVxcbiAgICAgICAgICAgICA7KHByb3BlcnR5IGNvbnNlcnZlcy1tYXNzKSBub3Qgc3VwcG9ydGVkIHlldFxcbiAgICAgICAgICAgXVxcblxcbiAgICAoc3RlcCAoYnV5LWdhcyBzZW5kZXIgdG90YWwpKVxcbiAgICAoc3RlcCAocmVkZWVtLWdhcyBtaW5lciBtaW5lci1ndWFyZCBzZW5kZXIgdG90YWwpKVxcbiAgICApXFxuXFxuICAoZGVmdW4gZGViaXQ6c3RyaW5nIChhY2NvdW50OnN0cmluZyBhbW91bnQ6ZGVjaW1hbClcXG4gICAgQGRvYyBcXFwiRGViaXQgQU1PVU5UIGZyb20gQUNDT1VOVCBiYWxhbmNlXFxcIlxcblxcbiAgICBAbW9kZWwgWyAocHJvcGVydHkgKD4gYW1vdW50IDAuMCkpXFxuICAgICAgICAgICAgIChwcm9wZXJ0eSAodmFsaWQtYWNjb3VudCBhY2NvdW50KSlcXG4gICAgICAgICAgIF1cXG5cXG4gICAgKHZhbGlkYXRlLWFjY291bnQgYWNjb3VudClcXG5cXG4gICAgKGVuZm9yY2UgKD4gYW1vdW50IDAuMClcXG4gICAgICBcXFwiZGViaXQgYW1vdW50IG11c3QgYmUgcG9zaXRpdmVcXFwiKVxcblxcbiAgICAoZW5mb3JjZS11bml0IGFtb3VudClcXG5cXG4gICAgKHJlcXVpcmUtY2FwYWJpbGl0eSAoREVCSVQgYWNjb3VudCkpXFxuICAgICh3aXRoLXJlYWQgY29pbi10YWJsZSBhY2NvdW50XFxuICAgICAgeyBcXFwiYmFsYW5jZVxcXCIgOj0gYmFsYW5jZSB9XFxuXFxuICAgICAgKGVuZm9yY2UgKDw9IGFtb3VudCBiYWxhbmNlKSBcXFwiSW5zdWZmaWNpZW50IGZ1bmRzXFxcIilcXG5cXG4gICAgICAodXBkYXRlIGNvaW4tdGFibGUgYWNjb3VudFxcbiAgICAgICAgeyBcXFwiYmFsYW5jZVxcXCIgOiAoLSBiYWxhbmNlIGFtb3VudCkgfVxcbiAgICAgICAgKSlcXG4gICAgKVxcblxcblxcbiAgKGRlZnVuIGNyZWRpdDpzdHJpbmcgKGFjY291bnQ6c3RyaW5nIGd1YXJkOmd1YXJkIGFtb3VudDpkZWNpbWFsKVxcbiAgICBAZG9jIFxcXCJDcmVkaXQgQU1PVU5UIHRvIEFDQ09VTlQgYmFsYW5jZVxcXCJcXG5cXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5ICg-IGFtb3VudCAwLjApKVxcbiAgICAgICAgICAgICAocHJvcGVydHkgKHZhbGlkLWFjY291bnQgYWNjb3VudCkpXFxuICAgICAgICAgICBdXFxuXFxuICAgICh2YWxpZGF0ZS1hY2NvdW50IGFjY291bnQpXFxuXFxuICAgIChlbmZvcmNlICg-IGFtb3VudCAwLjApIFxcXCJjcmVkaXQgYW1vdW50IG11c3QgYmUgcG9zaXRpdmVcXFwiKVxcbiAgICAoZW5mb3JjZS11bml0IGFtb3VudClcXG5cXG4gICAgKHJlcXVpcmUtY2FwYWJpbGl0eSAoQ1JFRElUIGFjY291bnQpKVxcbiAgICAod2l0aC1kZWZhdWx0LXJlYWQgY29pbi10YWJsZSBhY2NvdW50XFxuICAgICAgeyBcXFwiYmFsYW5jZVxcXCIgOiAwLjAsIFxcXCJndWFyZFxcXCIgOiBndWFyZCB9XFxuICAgICAgeyBcXFwiYmFsYW5jZVxcXCIgOj0gYmFsYW5jZSwgXFxcImd1YXJkXFxcIiA6PSByZXRnIH1cXG4gICAgICA7IHdlIGRvbid0IHdhbnQgdG8gb3ZlcndyaXRlIGFuIGV4aXN0aW5nIGd1YXJkIHdpdGggdGhlIHVzZXItc3VwcGxpZWQgb25lXFxuICAgICAgKGVuZm9yY2UgKD0gcmV0ZyBndWFyZClcXG4gICAgICAgIFxcXCJhY2NvdW50IGd1YXJkcyBkbyBub3QgbWF0Y2hcXFwiKVxcblxcbiAgICAgICh3cml0ZSBjb2luLXRhYmxlIGFjY291bnRcXG4gICAgICAgIHsgXFxcImJhbGFuY2VcXFwiIDogKCsgYmFsYW5jZSBhbW91bnQpXFxuICAgICAgICAsIFxcXCJndWFyZFxcXCIgICA6IHJldGdcXG4gICAgICAgIH0pXFxuICAgICAgKSlcXG5cXG5cXG4gIChkZWZzY2hlbWEgY3Jvc3NjaGFpbi1zY2hlbWFcXG4gICAgQGRvYyBcXFwiU2NoZW1hIGZvciB5aWVsZGVkIHZhbHVlIGluIGNyb3NzLWNoYWluIHRyYW5zZmVyc1xcXCJcXG4gICAgcmVjZWl2ZXI6c3RyaW5nXFxuICAgIHJlY2VpdmVyLWd1YXJkOmd1YXJkXFxuICAgIGFtb3VudDpkZWNpbWFsKVxcblxcbiAgKGRlZnBhY3QgdHJhbnNmZXItY3Jvc3NjaGFpbjpzdHJpbmdcXG4gICAgKCBzZW5kZXI6c3RyaW5nXFxuICAgICAgcmVjZWl2ZXI6c3RyaW5nXFxuICAgICAgcmVjZWl2ZXItZ3VhcmQ6Z3VhcmRcXG4gICAgICB0YXJnZXQtY2hhaW46c3RyaW5nXFxuICAgICAgYW1vdW50OmRlY2ltYWwgKVxcblxcbiAgICBAbW9kZWwgWyAocHJvcGVydHkgKD4gYW1vdW50IDAuMCkpXFxuICAgICAgICAgICAgIChwcm9wZXJ0eSAoIT0gcmVjZWl2ZXIgXFxcIlxcXCIpKVxcbiAgICAgICAgICAgICAocHJvcGVydHkgKHZhbGlkLWFjY291bnQgc2VuZGVyKSlcXG4gICAgICAgICAgICAgKHByb3BlcnR5ICh2YWxpZC1hY2NvdW50IHJlY2VpdmVyKSlcXG4gICAgICAgICAgIF1cXG5cXG4gICAgKHN0ZXBcXG4gICAgICAod2l0aC1jYXBhYmlsaXR5IChERUJJVCBzZW5kZXIpXFxuXFxuICAgICAgICAodmFsaWRhdGUtYWNjb3VudCBzZW5kZXIpXFxuICAgICAgICAodmFsaWRhdGUtYWNjb3VudCByZWNlaXZlcilcXG5cXG4gICAgICAgIChlbmZvcmNlICghPSBcXFwiXFxcIiB0YXJnZXQtY2hhaW4pIFxcXCJlbXB0eSB0YXJnZXQtY2hhaW5cXFwiKVxcbiAgICAgICAgKGVuZm9yY2UgKCE9IChhdCAnY2hhaW4taWQgKGNoYWluLWRhdGEpKSB0YXJnZXQtY2hhaW4pXFxuICAgICAgICAgIFxcXCJjYW5ub3QgcnVuIGNyb3NzLWNoYWluIHRyYW5zZmVycyB0byB0aGUgc2FtZSBjaGFpblxcXCIpXFxuXFxuICAgICAgICAoZW5mb3JjZSAoPiBhbW91bnQgMC4wKVxcbiAgICAgICAgICBcXFwidHJhbnNmZXIgcXVhbnRpdHkgbXVzdCBiZSBwb3NpdGl2ZVxcXCIpXFxuXFxuICAgICAgICAoZW5mb3JjZS11bml0IGFtb3VudClcXG5cXG4gICAgICAgIDs7IHN0ZXAgMSAtIGRlYml0IGRlbGV0ZS1hY2NvdW50IG9uIGN1cnJlbnQgY2hhaW5cXG4gICAgICAgIChkZWJpdCBzZW5kZXIgYW1vdW50KVxcblxcbiAgICAgICAgKGxldFxcbiAgICAgICAgICAoKGNyb3NzY2hhaW4tZGV0YWlsczpvYmplY3R7Y3Jvc3NjaGFpbi1zY2hlbWF9XFxuICAgICAgICAgICAgeyBcXFwicmVjZWl2ZXJcXFwiIDogcmVjZWl2ZXJcXG4gICAgICAgICAgICAsIFxcXCJyZWNlaXZlci1ndWFyZFxcXCIgOiByZWNlaXZlci1ndWFyZFxcbiAgICAgICAgICAgICwgXFxcImFtb3VudFxcXCIgOiBhbW91bnRcXG4gICAgICAgICAgICB9KSlcXG4gICAgICAgICAgKHlpZWxkIGNyb3NzY2hhaW4tZGV0YWlscyB0YXJnZXQtY2hhaW4pXFxuICAgICAgICAgICkpKVxcblxcbiAgICAoc3RlcFxcbiAgICAgIChyZXN1bWVcXG4gICAgICAgIHsgXFxcInJlY2VpdmVyXFxcIiA6PSByZWNlaXZlclxcbiAgICAgICAgLCBcXFwicmVjZWl2ZXItZ3VhcmRcXFwiIDo9IHJlY2VpdmVyLWd1YXJkXFxuICAgICAgICAsIFxcXCJhbW91bnRcXFwiIDo9IGFtb3VudFxcbiAgICAgICAgfVxcblxcbiAgICAgICAgOzsgc3RlcCAyIC0gY3JlZGl0IGNyZWF0ZSBhY2NvdW50IG9uIHRhcmdldCBjaGFpblxcbiAgICAgICAgKHdpdGgtY2FwYWJpbGl0eSAoQ1JFRElUIHJlY2VpdmVyKVxcbiAgICAgICAgICAoY3JlZGl0IHJlY2VpdmVyIHJlY2VpdmVyLWd1YXJkIGFtb3VudCkpXFxuICAgICAgICApKVxcbiAgICApXFxuXFxuXFxuICA7IC0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tXFxuICA7IENvaW4gYWxsb2NhdGlvbnNcXG5cXG4gIChkZWZzY2hlbWEgYWxsb2NhdGlvbi1zY2hlbWFcXG4gICAgQGRvYyBcXFwiR2VuZXNpcyBhbGxvY2F0aW9uIHJlZ2lzdHJ5XFxcIlxcbiAgICA7QG1vZGVsIFsgKGludmFyaWFudCAoPj0gYmFsYW5jZSAwLjApKSBdXFxuXFxuICAgIGJhbGFuY2U6ZGVjaW1hbFxcbiAgICBkYXRlOnRpbWVcXG4gICAgZ3VhcmQ6Z3VhcmRcXG4gICAgcmVkZWVtZWQ6Ym9vbClcXG5cXG4gIChkZWZ0YWJsZSBhbGxvY2F0aW9uLXRhYmxlOnthbGxvY2F0aW9uLXNjaGVtYX0pXFxuXFxuICAoZGVmdW4gY3JlYXRlLWFsbG9jYXRpb24tYWNjb3VudFxcbiAgICAoIGFjY291bnQ6c3RyaW5nXFxuICAgICAgZGF0ZTp0aW1lXFxuICAgICAga2V5c2V0LXJlZjpzdHJpbmdcXG4gICAgICBhbW91bnQ6ZGVjaW1hbFxcbiAgICApXFxuXFxuICAgIEBkb2MgXFxcIkFkZCBhbiBlbnRyeSB0byB0aGUgY29pbiBhbGxvY2F0aW9uIHRhYmxlLiBUaGlzIGZ1bmN0aW9uIFxcXFxcXG4gICAgICAgICBcXFxcYWxzbyBjcmVhdGVzIGEgY29ycmVzcG9uZGluZyBlbXB0eSBjb2luIGNvbnRyYWN0IGFjY291bnQgXFxcXFxcbiAgICAgICAgIFxcXFxvZiB0aGUgc2FtZSBuYW1lIGFuZCBndWFyZC4gUmVxdWlyZXMgR0VORVNJUyBjYXBhYmlsaXR5LiBcXFwiXFxuXFxuICAgIEBtb2RlbCBbIChwcm9wZXJ0eSAodmFsaWQtYWNjb3VudCBhY2NvdW50KSkgXVxcblxcbiAgICAocmVxdWlyZS1jYXBhYmlsaXR5IChHRU5FU0lTKSlcXG5cXG4gICAgKHZhbGlkYXRlLWFjY291bnQgYWNjb3VudClcXG4gICAgKGVuZm9yY2UgKD49IGFtb3VudCAwLjApXFxuICAgICAgXFxcImFsbG9jYXRpb24gYW1vdW50IG11c3QgYmUgbm9uLW5lZ2F0aXZlXFxcIilcXG5cXG4gICAgKGVuZm9yY2UtdW5pdCBhbW91bnQpXFxuXFxuICAgIChsZXRcXG4gICAgICAoKGd1YXJkOmd1YXJkIChrZXlzZXQtcmVmLWd1YXJkIGtleXNldC1yZWYpKSlcXG5cXG4gICAgICAoY3JlYXRlLWFjY291bnQgYWNjb3VudCBndWFyZClcXG5cXG4gICAgICAoaW5zZXJ0IGFsbG9jYXRpb24tdGFibGUgYWNjb3VudFxcbiAgICAgICAgeyBcXFwiYmFsYW5jZVxcXCIgOiBhbW91bnRcXG4gICAgICAgICwgXFxcImRhdGVcXFwiIDogZGF0ZVxcbiAgICAgICAgLCBcXFwiZ3VhcmRcXFwiIDogZ3VhcmRcXG4gICAgICAgICwgXFxcInJlZGVlbWVkXFxcIiA6IGZhbHNlXFxuICAgICAgICB9KSkpXFxuXFxuICAoZGVmdW4gcmVsZWFzZS1hbGxvY2F0aW9uXFxuICAgICggYWNjb3VudDpzdHJpbmcgKVxcblxcbiAgICBAZG9jIFxcXCJSZWxlYXNlIGZ1bmRzIGFzc29jaWF0ZWQgd2l0aCBhbGxvY2F0aW9uIEFDQ09VTlQgaW50byBtYWluIGxlZGdlci4gICBcXFxcXFxuICAgICAgICAgXFxcXEFDQ09VTlQgbXVzdCBhbHJlYWR5IGV4aXN0IGluIG1haW4gbGVkZ2VyLiBBbGxvY2F0aW9uIGlzIGRlYWN0aXZhdGVkIFxcXFxcXG4gICAgICAgICBcXFxcYWZ0ZXIgcmVsZWFzZS5cXFwiXFxuICAgIEBtb2RlbCBbIChwcm9wZXJ0eSAodmFsaWQtYWNjb3VudCBhY2NvdW50KSkgXVxcblxcbiAgICAodmFsaWRhdGUtYWNjb3VudCBhY2NvdW50KVxcblxcbiAgICAod2l0aC1yZWFkIGFsbG9jYXRpb24tdGFibGUgYWNjb3VudFxcbiAgICAgIHsgXFxcImJhbGFuY2VcXFwiIDo9IGJhbGFuY2VcXG4gICAgICAsIFxcXCJkYXRlXFxcIiA6PSByZWxlYXNlLXRpbWVcXG4gICAgICAsIFxcXCJyZWRlZW1lZFxcXCIgOj0gcmVkZWVtZWRcXG4gICAgICAsIFxcXCJndWFyZFxcXCIgOj0gZ3VhcmRcXG4gICAgICB9XFxuXFxuICAgICAgKGxldCAoKGN1cnItdGltZTp0aW1lIChhdCAnYmxvY2stdGltZSAoY2hhaW4tZGF0YSkpKSlcXG5cXG4gICAgICAgIChlbmZvcmNlIChub3QgcmVkZWVtZWQpXFxuICAgICAgICAgIFxcXCJhbGxvY2F0aW9uIGZ1bmRzIGhhdmUgYWxyZWFkeSBiZWVuIHJlZGVlbWVkXFxcIilcXG5cXG4gICAgICAgIChlbmZvcmNlXFxuICAgICAgICAgICg-PSBjdXJyLXRpbWUgcmVsZWFzZS10aW1lKVxcbiAgICAgICAgICAoZm9ybWF0IFxcXCJmdW5kcyBsb2NrZWQgdW50aWwge30uIGN1cnJlbnQgdGltZToge31cXFwiIFtyZWxlYXNlLXRpbWUgY3Vyci10aW1lXSkpXFxuXFxuICAgICAgICAoZW5mb3JjZS1ndWFyZCBndWFyZClcXG5cXG4gICAgICAgICh3aXRoLWNhcGFiaWxpdHkgKENSRURJVCBhY2NvdW50KVxcbiAgICAgICAgICAoY3JlZGl0IGFjY291bnQgZ3VhcmQgYmFsYW5jZSlcXG5cXG4gICAgICAgICAgKHVwZGF0ZSBhbGxvY2F0aW9uLXRhYmxlIGFjY291bnRcXG4gICAgICAgICAgICB7IFxcXCJyZWRlZW1lZFxcXCIgOiB0cnVlXFxuICAgICAgICAgICAgLCBcXFwiYmFsYW5jZVxcXCIgOiAwLjBcXG4gICAgICAgICAgICB9KVxcblxcbiAgICAgICAgICBcXFwiQWxsb2NhdGlvbiBzdWNjZXNzZnVsbHkgcmVsZWFzZWQgdG8gbWFpbiBsZWRnZXJcXFwiKVxcbiAgICApKSlcXG5cXG4pXFxuXFxuKGNyZWF0ZS10YWJsZSBjb2luLXRhYmxlKVxcbihjcmVhdGUtdGFibGUgYWxsb2NhdGlvbi10YWJsZSlcXG5cIn19LFwic2lnbmVyc1wiOltdLFwibWV0YVwiOntcImNyZWF0aW9uVGltZVwiOjAsXCJ0dGxcIjoxNzI4MDAsXCJnYXNMaW1pdFwiOjAsXCJjaGFpbklkXCI6XCJcIixcImdhc1ByaWNlXCI6MCxcInNlbmRlclwiOlwiXCJ9LFwibm9uY2VcIjpcIlxcXCJnZW5lc2lzLTAxXFxcIlwifSJ9
  - eyJnYXMiOjAsInJlc3VsdCI6eyJzdGF0dXMiOiJzdWNjZXNzIiwiZGF0YSI6IlRhYmxlQ3JlYXRlZCJ9LCJyZXFLZXkiOiI0ZlVmcVNXUzdHdTlGMzVWN08xV3dNZzF1bS1uNnpqZktpVFhhOWVPNHowIiwibG9ncyI6IlQxMWxnMTFJZVBLWHU4WXhXYW1DRXkyc0MzX1JuUW9GQXlzbGdySm1CS0EiLCJtZXRhRGF0YSI6bnVsbCwiY29udGludWF0aW9uIjpudWxsLCJ0eElkIjoxfQ
- - eyJoYXNoIjoic09zeDZkeWRwZkg2eXViNkM3ZDVUUVFLWEtmdkVDQUlZb0tGSXdNZlNpWSIsInNpZ3MiOltdLCJjbWQiOiJ7XCJuZXR3b3JrSWRcIjpudWxsLFwicGF5bG9hZFwiOntcImV4ZWNcIjp7XCJkYXRhXCI6e1wic2VuZGVyMDdcIjpbXCI0YzMxZGM5ZWU3ZjI0MTc3Zjc4YjZmNTE4MDEyYTIwODMyNmUyYWYxZjM3YmIwYTI0MDViNTA1NmQwY2FkNjI4XCJdLFwic2VuZGVyMDFcIjpbXCI2YmUyZjQ4NWE3YWY3NWZlZGI0YjdmMTUzYTkwM2Y3ZTYwMDBjYTRhYTUwMTE3OWM5MWEyNDUwYjc3N2JkMmE3XCJdLFwic2VuZGVyMDZcIjpbXCI1ZmZjMWY3ZmVmN2E0NDczODYyNTc2MmY3NWE0MjI5NDU0OTUxZTAzZjJhZmM2ZjgxMzA5YzBjMWJkZjllZTZmXCJdLFwic2VuZGVyMDBcIjpbXCIzNjg4MjBmODBjMzI0YmJjN2MyYjA2MTA2ODhhN2RhNDNlMzlmOTFkMTE4NzMyNjcxY2Q5Yzc1MDBmZjQzY2NhXCJdLFwic2VuZGVyMDVcIjpbXCJmMDlkOGY2Mzk0YWVhNDI1ZmU2NzgzZDg4Y2Q4MTM2M2Q4MDE3ZjE2YWZkMzcxMWM1NzViZTBmNWNkNWM5YmI5XCJdLFwic2VuZGVyMDRcIjpbXCIyZDcwYWE0ZjY5N2MzYTNiOGRkNmQ5Nzc0NWFjMDc0ZWRjZmQwZWI2NWMzNzc3NGNkZTI1MTM1NDgzYmVhNzFlXCJdLFwibXVsdGktMDItMDMtMDQtYW55XCI6e1wicHJlZFwiOlwia2V5cy1hbnlcIixcImtleXNcIjpbXCIzYTlkZDUzMmQ3M2RhY2UxOTVkYmI2NGQxZGJhNjU3MmZiNzgzZDBmZGQzMjQ2ODVlMzJmYmRhMmY4OWY5OWE2XCIsXCI0M2YyYWRiMWRlMTkyMDAwY2IzNzc3YmFjYzdmOTgzYjY2MTRmZDljMTcxNWNkNDRjZDQ4NGI2ZDNhMGQzNGM4XCIsXCIyZDcwYWE0ZjY5N2MzYTNiOGRkNmQ5Nzc0NWFjMDc0ZWRjZmQwZWI2NWMzNzc3NGNkZTI1MTM1NDgzYmVhNzFlXCJdfSxcInNlbmRlcjA5XCI6W1wiYzU5ZDk4NDBiMGI2NjA5MDgzNjU0NmI3ZWI0YTczNjA2MjU3NTI3ZWM4YzJiNDgyMzAwZmQyMjkyNjRiMDdlNlwiXSxcInNlbmRlcjAzXCI6W1wiNDNmMmFkYjFkZTE5MjAwMGNiMzc3N2JhY2M3Zjk4M2I2NjE0ZmQ5YzE3MTVjZDQ0Y2Q0ODRiNmQzYTBkMzRjOFwiXSxcIm11bHRpLTAwLTAxXCI6W1wiMzY4ODIwZjgwYzMyNGJiYzdjMmIwNjEwNjg4YTdkYTQzZTM5ZjkxZDExODczMjY3MWNkOWM3NTAwZmY0M2NjYVwiLFwiNmJlMmY0ODVhN2FmNzVmZWRiNGI3ZjE1M2E5MDNmN2U2MDAwY2E0YWE1MDExNzljOTFhMjQ1MGI3NzdiZDJhN1wiXSxcInNlbmRlcjA4XCI6W1wiNjNiMmViYTRlZDcwZDQ2MTJkM2U3YmM5MGRiMmZiZjRjNzZmN2IwNzQzNjNlODZkNzNmMGJjNjE3ZjhlOGI4MVwiXSxcInNlbmRlcjAyXCI6W1wiM2E5ZGQ1MzJkNzNkYWNlMTk1ZGJiNjRkMWRiYTY1NzJmYjc4M2QwZmRkMzI0Njg1ZTMyZmJkYTJmODlmOTlhNlwiXX0sXCJjb2RlXCI6XCIoY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDBcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDBcXFwiKSAxMDAwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDFcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDFcXFwiKSAxMTAwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDJcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDJcXFwiKSAxMjAwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDNcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDNcXFwiKSAxMzAwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDRcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDRcXFwiKSAxNDAwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDVcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDVcXFwiKSAxNTAwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDZcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDZcXFwiKSAxNjAwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDdcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDdcXFwiKSAxNzAwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDhcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDhcXFwiKSAxODAwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwic2VuZGVyMDlcXFwiIChyZWFkLWtleXNldCBcXFwic2VuZGVyMDlcXFwiKSAxOTAwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwibXVsdGktMDAtMDFcXFwiIChyZWFkLWtleXNldCBcXFwibXVsdGktMDAtMDFcXFwiKSAxMDEwMDAwMDAuMClcXG4oY29pbi5jb2luYmFzZSBcXFwibXVsdGktMDItMDMtMDQtYW55XFxcIiAocmVhZC1rZXlzZXQgXFxcIm11bHRpLTAyLTAzLTA0LWFueVxcXCIpIDEyMzQwMDAwMC4wKVwifX0sXCJzaWduZXJzXCI6W10sXCJtZXRhXCI6e1wiY3JlYXRpb25UaW1lXCI6MCxcInR0bFwiOjE3MjgwMCxcImdhc0xpbWl0XCI6MCxcImNoYWluSWRcIjpcIlwiLFwiZ2FzUHJpY2VcIjowLFwic2VuZGVyXCI6XCJcIn0sXCJub25jZVwiOlwiXFxcInRlc3RuZXQtZ3JhbnRzXFxcIlwifSJ9
  - eyJnYXMiOjAsInJlc3VsdCI6eyJzdGF0dXMiOiJzdWNjZXNzIiwiZGF0YSI6IldyaXRlIHN1Y2NlZWRlZCJ9LCJyZXFLZXkiOiJzT3N4NmR5ZHBmSDZ5dWI2QzdkNVRRUUtYS2Z2RUNBSVlvS0ZJd01mU2lZIiwibG9ncyI6IndOT0ItemVUbW81ZHkyVWJDWTM5cEgzaWw2SUFPM2ljeUxMclRCTzdxNlkiLCJtZXRhRGF0YSI6bnVsbCwiY29udGludWF0aW9uIjpudWxsLCJ0eElkIjoyfQ
- - eyJoYXNoIjoidV9Ob2tialZQSFVIckxsTDludE9mMHdiZTM4bDYzLWJuN3RxOVFMa1YycyIsInNpZ3MiOlt7InNpZyI6IjQxYTIzOTVmMWJiYTI4NzA0N2I1M2FjMTM4NzI5YjNmNGRhYmViMmVlNTM3ODQyNDkyNzE5NTNmN2FhOTcwOTA0Zjc4ZGYzYmRjNzNjOGU2ODM3M2UwYWVhYTIxMWQ4YzUyZTYxNTExZTA1NWI3ZDRhYzZlMjE1ZTEwZTMxNDBlIn1dLCJjbWQiOiJ7XCJuZXR3b3JrSWRcIjpudWxsLFwicGF5bG9hZFwiOntcImV4ZWNcIjp7XCJkYXRhXCI6e1wibnMtYWRtaW4ta2V5c2V0XCI6W1wiMzY4ODIwZjgwYzMyNGJiYzdjMmIwNjEwNjg4YTdkYTQzZTM5ZjkxZDExODczMjY3MWNkOWM3NTAwZmY0M2NjYVwiXSxcIm5zLW9wZXJhdGUta2V5c2V0XCI6W1wiMzY4ODIwZjgwYzMyNGJiYzdjMmIwNjEwNjg4YTdkYTQzZTM5ZjkxZDExODczMjY3MWNkOWM3NTAwZmY0M2NjYVwiXX0sXCJjb2RlXCI6XCJcXG4oZGVmaW5lLWtleXNldCAnbnMtYWRtaW4ta2V5c2V0IChyZWFkLWtleXNldCAnbnMtYWRtaW4ta2V5c2V0KSlcXG4oZGVmaW5lLWtleXNldCAnbnMtb3BlcmF0ZS1rZXlzZXQgKHJlYWQta2V5c2V0ICducy1vcGVyYXRlLWtleXNldCkpXFxuXFxuKG1vZHVsZSBucyAnbnMtYWRtaW4ta2V5c2V0XFxuICBcXFwiQWRtaW5pc3RlcnMgZGVmaW5pdGlvbiBvZiBuZXcgbmFtZXNwYWNlcyBpbiBDaGFpbndlYi5cXFwiXFxuXFxuICAoZGVmc2NoZW1hIHJlZy1lbnRyeVxcbiAgICBhZG1pbi1ndWFyZDpndWFyZFxcbiAgICBhY3RpdmU6Ym9vbClcXG5cXG4gIChkZWZ0YWJsZSByZWdpc3RyeTp7cmVnLWVudHJ5fSlcXG5cXG4gIChkZWZjYXAgT1BFUkFURSAoKVxcbiAgICAoZW5mb3JjZS1rZXlzZXQgJ25zLW9wZXJhdGUta2V5c2V0KSlcXG5cXG4gIChkZWZjb25zdCBHVUFSRF9TVUNDRVNTIChjcmVhdGUtdXNlci1ndWFyZCAoc3VjY2VzcykpKVxcbiAgKGRlZmNvbnN0IEdVQVJEX0ZBSUxVUkUgKGNyZWF0ZS11c2VyLWd1YXJkIChmYWlsdXJlKSkpXFxuXFxuICAoZGVmdW4gc3VjY2VzcyAoKVxcbiAgICB0cnVlKVxcbiAgKGRlZnVuIGZhaWx1cmUgKClcXG4gICAgKGVuZm9yY2UgZmFsc2UgXFxcIkRpc2FibGVkXFxcIikpXFxuXFxuICAoZGVmdW4gdmFsaWRhdGUtbmFtZSAobmFtZSlcXG4gICAgKGVuZm9yY2UgKCE9IFxcXCJcXFwiIG5hbWUpIFxcXCJFbXB0eSBuYW1lIG5vdCBhbGxvd2VkXFxcIilcXG4gICAgKGVuZm9yY2UgKDwgKGxlbmd0aCBuYW1lKSA2NCkgXFxcIk5hbWUgbXVzdCBiZSBsZXNzIHRoYW4gNjQgY2hhcmFjdGVycyBsb25nXFxcIilcXG4gICAgKGVuZm9yY2UgKGlzLWNoYXJzZXQgQ0hBUlNFVF9MQVRJTjEgbmFtZSlcXG4gICAgICAgICAgICAgXFxcIk5hbWUgbXVzdCBiZSBpbiBsYXRpbjEgY2hhcnNldFxcXCIpKVxcblxcbiAgKGRlZnVuIHZhbGlkYXRlOmJvb2xcXG4gICAgICAoIG5zLW5hbWU6c3RyaW5nXFxuICAgICAgICBucy1hZG1pbjpndWFyZFxcbiAgICAgICAgKVxcbiAgICBcXFwiIE1hbmFnZXMgbmFtZXNwYWNlIGluc3RhbGwgZm9yIENoYWlud2ViLiBSZXF1aXJlcyBhY3RpdmUgcm93IGluIHJlZ2lzdHJ5IFxcXFxcXG4gICAgXFxcXCBmb3IgTlMtTkFNRSB3aXRoIGd1YXJkIG1hdGNoaW5nIE5TLUFETUlOLlxcXCJcXG5cXG4gICAgKHZhbGlkYXRlLW5hbWUgbnMtbmFtZSlcXG5cXG4gICAgKHdpdGgtZGVmYXVsdC1yZWFkIHJlZ2lzdHJ5IG5zLW5hbWVcXG4gICAgICB7ICdhZG1pbi1ndWFyZCA6IG5zLWFkbWluXFxuICAgICAgLCAnYWN0aXZlIDogZmFsc2UgfVxcbiAgICAgIHsgJ2FkbWluLWd1YXJkIDo9IGFnXFxuICAgICAgLCAnYWN0aXZlIDo9IGlzLWFjdGl2ZSB9XFxuXFxuICAgICAgICAoZW5mb3JjZSBpcy1hY3RpdmUgXFxcIkluYWN0aXZlIG9yIHVucmVnaXN0ZXJlZCBuYW1lc3BhY2VcXFwiKVxcbiAgICAgICAgKGVuZm9yY2UgKD0gbnMtYWRtaW4gYWcpIFxcXCJBZG1pbiBndWFyZCBtdXN0IG1hdGNoIGd1YXJkIGluIHJlZ2lzdHJ5XFxcIilcXG5cXG4gICAgICAgIHRydWUpKVxcblxcbiAgKGRlZnVuIHdyaXRlLXJlZ2lzdHJ5OnN0cmluZ1xcbiAgICAgICggbnMtbmFtZTpzdHJpbmdcXG4gICAgICAgIGd1YXJkOmd1YXJkXFxuICAgICAgICBhY3RpdmU6Ym9vbFxcbiAgICAgICAgKVxcbiAgICBcXFwiIFdyaXRlIGVudHJ5IHdpdGggR1VBUkQgYW5kIEFDVElWRSBpbnRvIHJlZ2lzdHJ5IGZvciBOQU1FLiBcXFxcXFxuICAgIFxcXFwgR3VhcmRlZCBieSBvcGVyYXRlIGtleXNldC4gXFxcIlxcblxcbiAgICAod2l0aC1jYXBhYmlsaXR5IChPUEVSQVRFKVxcblxcbiAgICAgICh2YWxpZGF0ZS1uYW1lIG5zLW5hbWUpXFxuXFxuICAgICAgKHdyaXRlIHJlZ2lzdHJ5IG5zLW5hbWVcXG4gICAgICAgIHsgJ2FkbWluLWd1YXJkOiBndWFyZFxcbiAgICAgICAgLCAnYWN0aXZlOiBhY3RpdmUgfSlcXG5cXG4gICAgICBcXFwiUmVnaXN0ZXIgZW50cnkgd3JpdHRlblxcXCIpKVxcblxcbiAgKGRlZnVuIHF1ZXJ5Om9iamVjdHtyZWctZW50cnl9XFxuICAgICAgKCBucy1uYW1lOnN0cmluZyApXFxuICAgIChyZWFkIHJlZ2lzdHJ5IG5zLW5hbWUpKVxcblxcbiAgKVxcblxcbihjcmVhdGUtdGFibGUgcmVnaXN0cnkpXFxuXFxuKHdyaXRlLXJlZ2lzdHJ5IFxcXCJrYWRlbmFcXFwiXFxuICAoa2V5c2V0LXJlZi1ndWFyZCAnbnMtb3BlcmF0ZS1rZXlzZXQpIHRydWUpXFxuKHdyaXRlLXJlZ2lzdHJ5IFxcXCJ1c2VyXFxcIiBHVUFSRF9GQUlMVVJFIHRydWUpXFxuKHdyaXRlLXJlZ2lzdHJ5IFxcXCJmcmVlXFxcIiBHVUFSRF9GQUlMVVJFIHRydWUpXFxuXFxuKGRlZmluZS1uYW1lc3BhY2UgXFxcImthZGVuYVxcXCJcXG4gIChrZXlzZXQtcmVmLWd1YXJkICducy1vcGVyYXRlLWtleXNldClcXG4gIChrZXlzZXQtcmVmLWd1YXJkICducy1vcGVyYXRlLWtleXNldCkpXFxuXFxuKGRlZmluZS1uYW1lc3BhY2UgXFxcInVzZXJcXFwiIEdVQVJEX1NVQ0NFU1MgR1VBUkRfRkFJTFVSRSlcXG4oZGVmaW5lLW5hbWVzcGFjZSBcXFwiZnJlZVxcXCIgR1VBUkRfU1VDQ0VTUyBHVUFSRF9GQUlMVVJFKVxcblwifX0sXCJzaWduZXJzXCI6W3tcInB1YktleVwiOlwiMzY4ODIwZjgwYzMyNGJiYzdjMmIwNjEwNjg4YTdkYTQzZTM5ZjkxZDExODczMjY3MWNkOWM3NTAwZmY0M2NjYVwifV0sXCJtZXRhXCI6e1wiY3JlYXRpb25UaW1lXCI6MCxcInR0bFwiOjE3MjgwMCxcImdhc0xpbWl0XCI6MCxcImNoYWluSWRcIjpcIlwiLFwiZ2FzUHJpY2VcIjowLFwic2VuZGVyXCI6XCJcIn0sXCJub25jZVwiOlwiXFxcImxvYWQtbnMtdGVzdG5ldC1zZW5kZXIwMFxcXCJcIn0ifQ
  - eyJnYXMiOjAsInJlc3VsdCI6eyJzdGF0dXMiOiJzdWNjZXNzIiwiZGF0YSI6Ik5hbWVzcGFjZSBkZWZpbmVkOiBmcmVlIn0sInJlcUtleSI6InVfTm9rYmpWUEhVSHJMbEw5bnRPZjB3YmUzOGw2My1ibjd0cTlRTGtWMnMiLCJsb2dzIjoiRGpTbTlEWEZSZGo4RjVGSXpPOWpMNUVUbDFuNGhYN3c2NHhiRGRGb1VJayIsIm1ldGFEYXRhIjpudWxsLCJjb250aW51YXRpb24iOm51bGwsInR4SWQiOjN9
minerData: eyJhY2NvdW50IjoiTm9NaW5lciIsInByZWRpY2F0ZSI6IjwiLCJwdWJsaWMta2V5cyI6W119
transactionsHash: o6ZCdZVtXuwDpRwbVbNOAyIFYAZvvdBarUgAOuPmNE8
outputsHash: wAz2xmbxhdtczIh2fuZkTSSQ3Eq8aMTrIGatg-AOaDU
payloadHash: 5yVv9BqhVEWSJqoWmvZUHKQKXm2NsUTBJQK4vtKq1Gg
coinbase: eyJnYXMiOjAsInJlc3VsdCI6eyJzdGF0dXMiOiJzdWNjZXNzIiwiZGF0YSI6Ik5PX0NPSU5CQVNFIn0sInJlcUtleSI6IkRsZFJ3Q2JsUTdMb3F5NndZSm5hb2RIbDMwZDNqM2VILXF0RnpmRXY0NmciLCJsb2dzIjpudWxsLCJtZXRhRGF0YSI6bnVsbCwiY29udGludWF0aW9uIjpudWxsLCJ0eElkIjpudWxsfQ

|]
