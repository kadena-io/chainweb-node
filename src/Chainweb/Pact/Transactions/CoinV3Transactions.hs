{-# LANGUAGE OverloadedStrings #-}

-- This module is auto-generated. DO NOT EDIT IT MANUALLY.

module Chainweb.Pact.Transactions.CoinV3Transactions ( transactions ) where

import Data.Bifunctor (first)

import Chainweb.Transaction
import Chainweb.Utils

transactions :: IO [ChainwebTransaction]
transactions =
  let decodeTx t =
        fromEitherM . (first (userError . show)) . codecDecode chainwebPayloadCodec =<< decodeB64UrlNoPaddingText t
  in mapM decodeTx [
    "eyJoYXNoIjoiYzFLWFRpZmN6RXp3ekZ1enU1YmI2YUVhN19vTHRnN2VpZHdGUTZpOXlaSSIsInNpZ3MiOltdLCJjbWQiOiJ7XCJuZXR3b3JrSWRcIjpudWxsLFwicGF5bG9hZFwiOntcImV4ZWNcIjp7XCJkYXRhXCI6bnVsbCxcImNvZGVcIjpcIlxcbihtb2R1bGUgY29pbiBHT1ZFUk5BTkNFXFxuXFxuICBAZG9jIFxcXCInY29pbicgcmVwcmVzZW50cyB0aGUgS2FkZW5hIENvaW4gQ29udHJhY3QuIFRoaXMgY29udHJhY3QgcHJvdmlkZXMgYm90aCB0aGUgXFxcXFxcbiAgXFxcXGJ1eS9yZWRlZW0gZ2FzIHN1cHBvcnQgaW4gdGhlIGZvcm0gb2YgJ2Z1bmQtdHgnLCBhcyB3ZWxsIGFzIHRyYW5zZmVyLCAgICAgICBcXFxcXFxuICBcXFxcY3JlZGl0LCBkZWJpdCwgY29pbmJhc2UsIGFjY291bnQgY3JlYXRpb24gYW5kIHF1ZXJ5LCBhcyB3ZWxsIGFzIFNQViBidXJuICAgIFxcXFxcXG4gIFxcXFxjcmVhdGUuIFRvIGFjY2VzcyB0aGUgY29pbiBjb250cmFjdCwgeW91IG1heSB1c2UgaXRzIGZ1bGx5LXF1YWxpZmllZCBuYW1lLCAgXFxcXFxcbiAgXFxcXG9yIGlzc3VlIHRoZSAnKHVzZSBjb2luKScgY29tbWFuZCBpbiB0aGUgYm9keSBvZiBhIG1vZHVsZSBkZWNsYXJhdGlvbi5cXFwiXFxuXFxuICBAbW9kZWxcXG4gICAgWyAoZGVmcHJvcGVydHkgY29uc2VydmVzLW1hc3NcXG4gICAgICAgICg9IChjb2x1bW4tZGVsdGEgY29pbi10YWJsZSAnYmFsYW5jZSkgMC4wKSlcXG5cXG4gICAgICAoZGVmcHJvcGVydHkgdmFsaWQtYWNjb3VudCAoYWNjb3VudDpzdHJpbmcpXFxuICAgICAgICAoYW5kXFxuICAgICAgICAgICg-PSAobGVuZ3RoIGFjY291bnQpIDMpXFxuICAgICAgICAgICg8PSAobGVuZ3RoIGFjY291bnQpIDI1NikpKVxcbiAgICBdXFxuXFxuICAoaW1wbGVtZW50cyBmdW5naWJsZS12MilcXG5cXG4gIChibGVzcyBcXFwidXRfSl9aTmtveWFQVUVKaGl3VmVXbmtTUW45SlQ5c1FDV0tkampWVnJXb1xcXCIpXFxuXFxuICA7IC0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tXFxuICA7IFNjaGVtYXMgYW5kIFRhYmxlc1xcblxcbiAgKGRlZnNjaGVtYSBjb2luLXNjaGVtYVxcbiAgICBAZG9jIFxcXCJUaGUgY29pbiBjb250cmFjdCB0b2tlbiBzY2hlbWFcXFwiXFxuICAgIEBtb2RlbCBbIChpbnZhcmlhbnQgKD49IGJhbGFuY2UgMC4wKSkgXVxcblxcbiAgICBiYWxhbmNlOmRlY2ltYWxcXG4gICAgZ3VhcmQ6Z3VhcmQpXFxuXFxuICAoZGVmdGFibGUgY29pbi10YWJsZTp7Y29pbi1zY2hlbWF9KVxcblxcbiAgOyAtLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLVxcbiAgOyBDYXBhYmlsaXRpZXNcXG5cXG4gIChkZWZjYXAgR09WRVJOQU5DRSAoKVxcbiAgICAoZW5mb3JjZSBmYWxzZSBcXFwiRW5mb3JjZSBub24tdXBncmFkZWFiaWxpdHlcXFwiKSlcXG5cXG4gIChkZWZjYXAgR0FTICgpXFxuICAgIFxcXCJNYWdpYyBjYXBhYmlsaXR5IHRvIHByb3RlY3QgZ2FzIGJ1eSBhbmQgcmVkZWVtXFxcIlxcbiAgICB0cnVlKVxcblxcbiAgKGRlZmNhcCBDT0lOQkFTRSAoKVxcbiAgICBcXFwiTWFnaWMgY2FwYWJpbGl0eSB0byBwcm90ZWN0IG1pbmVyIHJld2FyZFxcXCJcXG4gICAgdHJ1ZSlcXG5cXG4gIChkZWZjYXAgR0VORVNJUyAoKVxcbiAgICBcXFwiTWFnaWMgY2FwYWJpbGl0eSBjb25zdHJhaW5pbmcgZ2VuZXNpcyB0cmFuc2FjdGlvbnNcXFwiXFxuICAgIHRydWUpXFxuXFxuICAoZGVmY2FwIFJFTUVESUFURSAoKVxcbiAgICBcXFwiTWFnaWMgY2FwYWJpbGl0eSBmb3IgcmVtZWRpYXRpb24gdHJhbnNhY3Rpb25zXFxcIlxcbiAgICB0cnVlKVxcblxcbiAgKGRlZmNhcCBERUJJVCAoc2VuZGVyOnN0cmluZylcXG4gICAgXFxcIkNhcGFiaWxpdHkgZm9yIG1hbmFnaW5nIGRlYml0aW5nIG9wZXJhdGlvbnNcXFwiXFxuICAgIChlbmZvcmNlLWd1YXJkIChhdCAnZ3VhcmQgKHJlYWQgY29pbi10YWJsZSBzZW5kZXIpKSlcXG4gICAgKGVuZm9yY2UgKCE9IHNlbmRlciBcXFwiXFxcIikgXFxcInZhbGlkIHNlbmRlclxcXCIpKVxcblxcbiAgKGRlZmNhcCBDUkVESVQgKHJlY2VpdmVyOnN0cmluZylcXG4gICAgXFxcIkNhcGFiaWxpdHkgZm9yIG1hbmFnaW5nIGNyZWRpdGluZyBvcGVyYXRpb25zXFxcIlxcbiAgICAoZW5mb3JjZSAoIT0gcmVjZWl2ZXIgXFxcIlxcXCIpIFxcXCJ2YWxpZCByZWNlaXZlclxcXCIpKVxcblxcbiAgKGRlZmNhcCBST1RBVEUgKGFjY291bnQ6c3RyaW5nKVxcbiAgICBAZG9jIFxcXCJBdXRvbm9tb3VzbHkgbWFuYWdlZCBjYXBhYmlsaXR5IGZvciBndWFyZCByb3RhdGlvblxcXCJcXG4gICAgQG1hbmFnZWRcXG4gICAgdHJ1ZSlcXG5cXG4gIChkZWZjYXAgVFJBTlNGRVI6Ym9vbFxcbiAgICAoIHNlbmRlcjpzdHJpbmdcXG4gICAgICByZWNlaXZlcjpzdHJpbmdcXG4gICAgICBhbW91bnQ6ZGVjaW1hbFxcbiAgICApXFxuICAgIEBtYW5hZ2VkIGFtb3VudCBUUkFOU0ZFUi1tZ3JcXG4gICAgKGVuZm9yY2UgKCE9IHNlbmRlciByZWNlaXZlcikgXFxcInNhbWUgc2VuZGVyIGFuZCByZWNlaXZlclxcXCIpXFxuICAgIChlbmZvcmNlLXVuaXQgYW1vdW50KVxcbiAgICAoZW5mb3JjZSAoPiBhbW91bnQgMC4wKSBcXFwiUG9zaXRpdmUgYW1vdW50XFxcIilcXG4gICAgKGNvbXBvc2UtY2FwYWJpbGl0eSAoREVCSVQgc2VuZGVyKSlcXG4gICAgKGNvbXBvc2UtY2FwYWJpbGl0eSAoQ1JFRElUIHJlY2VpdmVyKSlcXG4gIClcXG5cXG4gIChkZWZ1biBUUkFOU0ZFUi1tZ3I6ZGVjaW1hbFxcbiAgICAoIG1hbmFnZWQ6ZGVjaW1hbFxcbiAgICAgIHJlcXVlc3RlZDpkZWNpbWFsXFxuICAgIClcXG5cXG4gICAgKGxldCAoKG5ld2JhbCAoLSBtYW5hZ2VkIHJlcXVlc3RlZCkpKVxcbiAgICAgIChlbmZvcmNlICg-PSBuZXdiYWwgMC4wKVxcbiAgICAgICAgKGZvcm1hdCBcXFwiVFJBTlNGRVIgZXhjZWVkZWQgZm9yIGJhbGFuY2Uge31cXFwiIFttYW5hZ2VkXSkpXFxuICAgICAgbmV3YmFsKVxcbiAgKVxcblxcbiAgOyB2MyBjYXBhYmlsaXRpZXNcXG4gIChkZWZjYXAgUkVMRUFTRV9BTExPQ0FUSU9OXFxuICAgICggYWNjb3VudDpzdHJpbmdcXG4gICAgICBhbW91bnQ6ZGVjaW1hbFxcbiAgICApXFxuICAgIEBkb2MgXFxcIkV2ZW50IGZvciBhbGxvY2F0aW9uIHJlbGVhc2UsIGNhbiBiZSB1c2VkIGZvciBzaWcgc2NvcGluZy5cXFwiXFxuICAgIEBldmVudCB0cnVlXFxuICApXFxuXFxuICA7IC0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tXFxuICA7IENvbnN0YW50c1xcblxcbiAgKGRlZmNvbnN0IENPSU5fQ0hBUlNFVCBDSEFSU0VUX0xBVElOMVxcbiAgICBcXFwiVGhlIGRlZmF1bHQgY29pbiBjb250cmFjdCBjaGFyYWN0ZXIgc2V0XFxcIilcXG5cXG4gIChkZWZjb25zdCBNSU5JTVVNX1BSRUNJU0lPTiAxMlxcbiAgICBcXFwiTWluaW11bSBhbGxvd2VkIHByZWNpc2lvbiBmb3IgY29pbiB0cmFuc2FjdGlvbnNcXFwiKVxcblxcbiAgKGRlZmNvbnN0IE1JTklNVU1fQUNDT1VOVF9MRU5HVEggM1xcbiAgICBcXFwiTWluaW11bSBhY2NvdW50IGxlbmd0aCBhZG1pc3NpYmxlIGZvciBjb2luIGFjY291bnRzXFxcIilcXG5cXG4gIChkZWZjb25zdCBNQVhJTVVNX0FDQ09VTlRfTEVOR1RIIDI1NlxcbiAgICBcXFwiTWF4aW11bSBhY2NvdW50IG5hbWUgbGVuZ3RoIGFkbWlzc2libGUgZm9yIGNvaW4gYWNjb3VudHNcXFwiKVxcblxcbiAgOyAtLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLVxcbiAgOyBVdGlsaXRpZXNcXG5cXG4gIChkZWZ1biBlbmZvcmNlLXVuaXQ6Ym9vbCAoYW1vdW50OmRlY2ltYWwpXFxuICAgIEBkb2MgXFxcIkVuZm9yY2UgbWluaW11bSBwcmVjaXNpb24gYWxsb3dlZCBmb3IgY29pbiB0cmFuc2FjdGlvbnNcXFwiXFxuXFxuICAgIChlbmZvcmNlXFxuICAgICAgKD0gKGZsb29yIGFtb3VudCBNSU5JTVVNX1BSRUNJU0lPTilcXG4gICAgICAgICBhbW91bnQpXFxuICAgICAgKGZvcm1hdCBcXFwiQW1vdW50IHZpb2xhdGVzIG1pbmltdW0gcHJlY2lzaW9uOiB7fVxcXCIgW2Ftb3VudF0pKVxcbiAgICApXFxuXFxuICAoZGVmdW4gdmFsaWRhdGUtYWNjb3VudCAoYWNjb3VudDpzdHJpbmcpXFxuICAgIEBkb2MgXFxcIkVuZm9yY2UgdGhhdCBhbiBhY2NvdW50IG5hbWUgY29uZm9ybXMgdG8gdGhlIGNvaW4gY29udHJhY3QgXFxcXFxcbiAgICAgICAgIFxcXFxtaW5pbXVtIGFuZCBtYXhpbXVtIGxlbmd0aCByZXF1aXJlbWVudHMsIGFzIHdlbGwgYXMgdGhlICAgIFxcXFxcXG4gICAgICAgICBcXFxcbGF0aW4tMSBjaGFyYWN0ZXIgc2V0LlxcXCJcXG5cXG4gICAgKGVuZm9yY2VcXG4gICAgICAoaXMtY2hhcnNldCBDT0lOX0NIQVJTRVQgYWNjb3VudClcXG4gICAgICAoZm9ybWF0XFxuICAgICAgICBcXFwiQWNjb3VudCBkb2VzIG5vdCBjb25mb3JtIHRvIHRoZSBjb2luIGNvbnRyYWN0IGNoYXJzZXQ6IHt9XFxcIlxcbiAgICAgICAgW2FjY291bnRdKSlcXG5cXG4gICAgKGxldCAoKGFjY291bnQtbGVuZ3RoIChsZW5ndGggYWNjb3VudCkpKVxcblxcbiAgICAgIChlbmZvcmNlXFxuICAgICAgICAoPj0gYWNjb3VudC1sZW5ndGggTUlOSU1VTV9BQ0NPVU5UX0xFTkdUSClcXG4gICAgICAgIChmb3JtYXRcXG4gICAgICAgICAgXFxcIkFjY291bnQgbmFtZSBkb2VzIG5vdCBjb25mb3JtIHRvIHRoZSBtaW4gbGVuZ3RoIHJlcXVpcmVtZW50OiB7fVxcXCJcXG4gICAgICAgICAgW2FjY291bnRdKSlcXG5cXG4gICAgICAoZW5mb3JjZVxcbiAgICAgICAgKDw9IGFjY291bnQtbGVuZ3RoIE1BWElNVU1fQUNDT1VOVF9MRU5HVEgpXFxuICAgICAgICAoZm9ybWF0XFxuICAgICAgICAgIFxcXCJBY2NvdW50IG5hbWUgZG9lcyBub3QgY29uZm9ybSB0byB0aGUgbWF4IGxlbmd0aCByZXF1aXJlbWVudDoge31cXFwiXFxuICAgICAgICAgIFthY2NvdW50XSkpXFxuICAgICAgKVxcbiAgKVxcblxcbiAgOyAtLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLVxcbiAgOyBDb2luIENvbnRyYWN0XFxuXFxuICAoZGVmdW4gZ2FzLW9ubHkgKClcXG4gICAgXFxcIlByZWRpY2F0ZSBmb3IgZ2FzLW9ubHkgdXNlciBndWFyZHMuXFxcIlxcbiAgICAocmVxdWlyZS1jYXBhYmlsaXR5IChHQVMpKSlcXG5cXG4gIChkZWZ1biBnYXMtZ3VhcmQgKGd1YXJkOmd1YXJkKVxcbiAgICBcXFwiUHJlZGljYXRlIGZvciBnYXMgKyBzaW5nbGUga2V5IHVzZXIgZ3VhcmRzXFxcIlxcbiAgICAoZW5mb3JjZS1vbmVcXG4gICAgICBcXFwiRW5mb3JjZSBlaXRoZXIgdGhlIHByZXNlbmNlIG9mIGEgR0FTIGNhcCBvciBrZXlzZXRcXFwiXFxuICAgICAgWyAoZ2FzLW9ubHkpXFxuICAgICAgICAoZW5mb3JjZS1ndWFyZCBndWFyZClcXG4gICAgICBdKSlcXG5cXG4gIChkZWZ1biBidXktZ2FzOnN0cmluZyAoc2VuZGVyOnN0cmluZyB0b3RhbDpkZWNpbWFsKVxcbiAgICBAZG9jIFxcXCJUaGlzIGZ1bmN0aW9uIGRlc2NyaWJlcyB0aGUgbWFpbiAnZ2FzIGJ1eScgb3BlcmF0aW9uLiBBdCB0aGlzIHBvaW50IFxcXFxcXG4gICAgXFxcXE1JTkVSIGhhcyBiZWVuIGNob3NlbiBmcm9tIHRoZSBwb29sLCBhbmQgd2lsbCBiZSB2YWxpZGF0ZWQuIFRoZSBTRU5ERVIgICBcXFxcXFxuICAgIFxcXFxvZiB0aGlzIHRyYW5zYWN0aW9uIGhhcyBzcGVjaWZpZWQgYSBnYXMgbGltaXQgTElNSVQgKG1heGltdW0gZ2FzKSBmb3IgICAgXFxcXFxcbiAgICBcXFxcdGhlIHRyYW5zYWN0aW9uLCBhbmQgdGhlIHByaWNlIGlzIHRoZSBzcG90IHByaWNlIG9mIGdhcyBhdCB0aGF0IHRpbWUuICAgIFxcXFxcXG4gICAgXFxcXFRoZSBnYXMgYnV5IHdpbGwgYmUgZXhlY3V0ZWQgcHJpb3IgdG8gZXhlY3V0aW5nIFNFTkRFUidzIGNvZGUuXFxcIlxcblxcbiAgICBAbW9kZWwgWyAocHJvcGVydHkgKD4gdG90YWwgMC4wKSlcXG4gICAgICAgICAgICAgKHByb3BlcnR5ICh2YWxpZC1hY2NvdW50IHNlbmRlcikpXFxuICAgICAgICAgICBdXFxuXFxuICAgICh2YWxpZGF0ZS1hY2NvdW50IHNlbmRlcilcXG5cXG4gICAgKGVuZm9yY2UtdW5pdCB0b3RhbClcXG4gICAgKGVuZm9yY2UgKD4gdG90YWwgMC4wKSBcXFwiZ2FzIHN1cHBseSBtdXN0IGJlIGEgcG9zaXRpdmUgcXVhbnRpdHlcXFwiKVxcblxcbiAgICAocmVxdWlyZS1jYXBhYmlsaXR5IChHQVMpKVxcbiAgICAod2l0aC1jYXBhYmlsaXR5IChERUJJVCBzZW5kZXIpXFxuICAgICAgKGRlYml0IHNlbmRlciB0b3RhbCkpXFxuICAgIClcXG5cXG4gIChkZWZ1biByZWRlZW0tZ2FzOnN0cmluZyAobWluZXI6c3RyaW5nIG1pbmVyLWd1YXJkOmd1YXJkIHNlbmRlcjpzdHJpbmcgdG90YWw6ZGVjaW1hbClcXG4gICAgQGRvYyBcXFwiVGhpcyBmdW5jdGlvbiBkZXNjcmliZXMgdGhlIG1haW4gJ3JlZGVlbSBnYXMnIG9wZXJhdGlvbi4gQXQgdGhpcyAgICBcXFxcXFxuICAgIFxcXFxwb2ludCwgdGhlIFNFTkRFUidzIHRyYW5zYWN0aW9uIGhhcyBiZWVuIGV4ZWN1dGVkLCBhbmQgdGhlIGdhcyB0aGF0ICAgICAgXFxcXFxcbiAgICBcXFxcd2FzIGNoYXJnZWQgaGFzIGJlZW4gY2FsY3VsYXRlZC4gTUlORVIgd2lsbCBiZSBjcmVkaXRlZCB0aGUgZ2FzIGNvc3QsICAgIFxcXFxcXG4gICAgXFxcXGFuZCBTRU5ERVIgd2lsbCByZWNlaXZlIHRoZSByZW1haW5kZXIgdXAgdG8gdGhlIGxpbWl0XFxcIlxcblxcbiAgICBAbW9kZWwgWyAocHJvcGVydHkgKD4gdG90YWwgMC4wKSlcXG4gICAgICAgICAgICAgKHByb3BlcnR5ICh2YWxpZC1hY2NvdW50IHNlbmRlcikpXFxuICAgICAgICAgICAgIChwcm9wZXJ0eSAodmFsaWQtYWNjb3VudCBtaW5lcikpXFxuICAgICAgICAgICBdXFxuXFxuICAgICh2YWxpZGF0ZS1hY2NvdW50IHNlbmRlcilcXG4gICAgKHZhbGlkYXRlLWFjY291bnQgbWluZXIpXFxuICAgIChlbmZvcmNlLXVuaXQgdG90YWwpXFxuXFxuICAgIChyZXF1aXJlLWNhcGFiaWxpdHkgKEdBUykpXFxuICAgIChsZXQqXFxuICAgICAgKChmZWUgKHJlYWQtZGVjaW1hbCBcXFwiZmVlXFxcIikpXFxuICAgICAgIChyZWZ1bmQgKC0gdG90YWwgZmVlKSkpXFxuXFxuICAgICAgKGVuZm9yY2UtdW5pdCBmZWUpXFxuICAgICAgKGVuZm9yY2UgKD49IGZlZSAwLjApXFxuICAgICAgICBcXFwiZmVlIG11c3QgYmUgYSBub24tbmVnYXRpdmUgcXVhbnRpdHlcXFwiKVxcblxcbiAgICAgIChlbmZvcmNlICg-PSByZWZ1bmQgMC4wKVxcbiAgICAgICAgXFxcInJlZnVuZCBtdXN0IGJlIGEgbm9uLW5lZ2F0aXZlIHF1YW50aXR5XFxcIilcXG5cXG4gICAgICAoZW1pdC1ldmVudCAoVFJBTlNGRVIgc2VuZGVyIG1pbmVyIGZlZSkpIDt2M1xcblxcbiAgICAgICAgOyBkaXJlY3RseSB1cGRhdGUgaW5zdGVhZCBvZiBjcmVkaXRcXG4gICAgICAod2l0aC1jYXBhYmlsaXR5IChDUkVESVQgc2VuZGVyKVxcbiAgICAgICAgKGlmICg-IHJlZnVuZCAwLjApXFxuICAgICAgICAgICh3aXRoLXJlYWQgY29pbi10YWJsZSBzZW5kZXJcXG4gICAgICAgICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6PSBiYWxhbmNlIH1cXG4gICAgICAgICAgICAodXBkYXRlIGNvaW4tdGFibGUgc2VuZGVyXFxuICAgICAgICAgICAgICB7IFxcXCJiYWxhbmNlXFxcIjogKCsgYmFsYW5jZSByZWZ1bmQpIH0pKVxcblxcbiAgICAgICAgICBcXFwibm9vcFxcXCIpKVxcblxcbiAgICAgICh3aXRoLWNhcGFiaWxpdHkgKENSRURJVCBtaW5lcilcXG4gICAgICAgIChpZiAoPiBmZWUgMC4wKVxcbiAgICAgICAgICAoY3JlZGl0IG1pbmVyIG1pbmVyLWd1YXJkIGZlZSlcXG4gICAgICAgICAgXFxcIm5vb3BcXFwiKSlcXG4gICAgICApXFxuXFxuICAgIClcXG5cXG4gIChkZWZ1biBjcmVhdGUtYWNjb3VudDpzdHJpbmcgKGFjY291bnQ6c3RyaW5nIGd1YXJkOmd1YXJkKVxcbiAgICBAbW9kZWwgWyAocHJvcGVydHkgKHZhbGlkLWFjY291bnQgYWNjb3VudCkpIF1cXG5cXG4gICAgKHZhbGlkYXRlLWFjY291bnQgYWNjb3VudClcXG4gICAgKGVuZm9yY2UtY2hhaW53ZWItYWNjb3VudCBhY2NvdW50IGd1YXJkKVxcblxcbiAgICAoaW5zZXJ0IGNvaW4tdGFibGUgYWNjb3VudFxcbiAgICAgIHsgXFxcImJhbGFuY2VcXFwiIDogMC4wXFxuICAgICAgLCBcXFwiZ3VhcmRcXFwiICAgOiBndWFyZFxcbiAgICAgIH0pXFxuICAgIClcXG5cXG4gIChkZWZ1biBnZXQtYmFsYW5jZTpkZWNpbWFsIChhY2NvdW50OnN0cmluZylcXG4gICAgKHdpdGgtcmVhZCBjb2luLXRhYmxlIGFjY291bnRcXG4gICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6PSBiYWxhbmNlIH1cXG4gICAgICBiYWxhbmNlXFxuICAgICAgKVxcbiAgICApXFxuXFxuICAoZGVmdW4gZGV0YWlsczpvYmplY3R7ZnVuZ2libGUtdjIuYWNjb3VudC1kZXRhaWxzfVxcbiAgICAoIGFjY291bnQ6c3RyaW5nIClcXG4gICAgKHdpdGgtcmVhZCBjb2luLXRhYmxlIGFjY291bnRcXG4gICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6PSBiYWxcXG4gICAgICAsIFxcXCJndWFyZFxcXCIgOj0gZyB9XFxuICAgICAgeyBcXFwiYWNjb3VudFxcXCIgOiBhY2NvdW50XFxuICAgICAgLCBcXFwiYmFsYW5jZVxcXCIgOiBiYWxcXG4gICAgICAsIFxcXCJndWFyZFxcXCI6IGcgfSlcXG4gICAgKVxcblxcbiAgKGRlZnVuIHJvdGF0ZTpzdHJpbmcgKGFjY291bnQ6c3RyaW5nIG5ldy1ndWFyZDpndWFyZClcXG4gICAgKHdpdGgtY2FwYWJpbGl0eSAoUk9UQVRFIGFjY291bnQpXFxuICAgICAgKHdpdGgtcmVhZCBjb2luLXRhYmxlIGFjY291bnRcXG4gICAgICAgIHsgXFxcImd1YXJkXFxcIiA6PSBvbGQtZ3VhcmQgfVxcblxcbiAgICAgICAgKGVuZm9yY2UtZ3VhcmQgb2xkLWd1YXJkKVxcblxcbiAgICAgICAgKHVwZGF0ZSBjb2luLXRhYmxlIGFjY291bnRcXG4gICAgICAgICAgeyBcXFwiZ3VhcmRcXFwiIDogbmV3LWd1YXJkIH1cXG4gICAgICAgICAgKSkpXFxuICAgIClcXG5cXG5cXG4gIChkZWZ1biBwcmVjaXNpb246aW50ZWdlclxcbiAgICAoKVxcbiAgICBNSU5JTVVNX1BSRUNJU0lPTilcXG5cXG4gIChkZWZ1biB0cmFuc2ZlcjpzdHJpbmcgKHNlbmRlcjpzdHJpbmcgcmVjZWl2ZXI6c3RyaW5nIGFtb3VudDpkZWNpbWFsKVxcbiAgICBAbW9kZWwgWyAocHJvcGVydHkgY29uc2VydmVzLW1hc3MpXFxuICAgICAgICAgICAgIChwcm9wZXJ0eSAoPiBhbW91bnQgMC4wKSlcXG4gICAgICAgICAgICAgKHByb3BlcnR5ICh2YWxpZC1hY2NvdW50IHNlbmRlcikpXFxuICAgICAgICAgICAgIChwcm9wZXJ0eSAodmFsaWQtYWNjb3VudCByZWNlaXZlcikpXFxuICAgICAgICAgICAgIChwcm9wZXJ0eSAoIT0gc2VuZGVyIHJlY2VpdmVyKSkgXVxcblxcbiAgICAoZW5mb3JjZSAoIT0gc2VuZGVyIHJlY2VpdmVyKVxcbiAgICAgIFxcXCJzZW5kZXIgY2Fubm90IGJlIHRoZSByZWNlaXZlciBvZiBhIHRyYW5zZmVyXFxcIilcXG5cXG4gICAgKHZhbGlkYXRlLWFjY291bnQgc2VuZGVyKVxcbiAgICAodmFsaWRhdGUtYWNjb3VudCByZWNlaXZlcilcXG5cXG4gICAgKGVuZm9yY2UgKD4gYW1vdW50IDAuMClcXG4gICAgICBcXFwidHJhbnNmZXIgYW1vdW50IG11c3QgYmUgcG9zaXRpdmVcXFwiKVxcblxcbiAgICAoZW5mb3JjZS11bml0IGFtb3VudClcXG5cXG4gICAgKHdpdGgtY2FwYWJpbGl0eSAoVFJBTlNGRVIgc2VuZGVyIHJlY2VpdmVyIGFtb3VudClcXG4gICAgICAoZGViaXQgc2VuZGVyIGFtb3VudClcXG4gICAgICAod2l0aC1yZWFkIGNvaW4tdGFibGUgcmVjZWl2ZXJcXG4gICAgICAgIHsgXFxcImd1YXJkXFxcIiA6PSBnIH1cXG5cXG4gICAgICAgIChjcmVkaXQgcmVjZWl2ZXIgZyBhbW91bnQpKVxcbiAgICAgIClcXG4gICAgKVxcblxcbiAgKGRlZnVuIHRyYW5zZmVyLWNyZWF0ZTpzdHJpbmdcXG4gICAgKCBzZW5kZXI6c3RyaW5nXFxuICAgICAgcmVjZWl2ZXI6c3RyaW5nXFxuICAgICAgcmVjZWl2ZXItZ3VhcmQ6Z3VhcmRcXG4gICAgICBhbW91bnQ6ZGVjaW1hbCApXFxuXFxuICAgIEBtb2RlbCBbIChwcm9wZXJ0eSBjb25zZXJ2ZXMtbWFzcykgXVxcblxcbiAgICAoZW5mb3JjZSAoIT0gc2VuZGVyIHJlY2VpdmVyKVxcbiAgICAgIFxcXCJzZW5kZXIgY2Fubm90IGJlIHRoZSByZWNlaXZlciBvZiBhIHRyYW5zZmVyXFxcIilcXG5cXG4gICAgKHZhbGlkYXRlLWFjY291bnQgc2VuZGVyKVxcbiAgICAodmFsaWRhdGUtYWNjb3VudCByZWNlaXZlcilcXG5cXG4gICAgKGVuZm9yY2UgKD4gYW1vdW50IDAuMClcXG4gICAgICBcXFwidHJhbnNmZXIgYW1vdW50IG11c3QgYmUgcG9zaXRpdmVcXFwiKVxcblxcbiAgICAoZW5mb3JjZS11bml0IGFtb3VudClcXG5cXG4gICAgKHdpdGgtY2FwYWJpbGl0eSAoVFJBTlNGRVIgc2VuZGVyIHJlY2VpdmVyIGFtb3VudClcXG4gICAgICAoZGViaXQgc2VuZGVyIGFtb3VudClcXG4gICAgICAoY3JlZGl0IHJlY2VpdmVyIHJlY2VpdmVyLWd1YXJkIGFtb3VudCkpXFxuICAgIClcXG5cXG4gIChkZWZ1biBjb2luYmFzZTpzdHJpbmcgKGFjY291bnQ6c3RyaW5nIGFjY291bnQtZ3VhcmQ6Z3VhcmQgYW1vdW50OmRlY2ltYWwpXFxuICAgIEBkb2MgXFxcIkludGVybmFsIGZ1bmN0aW9uIGZvciB0aGUgaW5pdGlhbCBjcmVhdGlvbiBvZiBjb2lucy4gIFRoaXMgZnVuY3Rpb24gXFxcXFxcbiAgICBcXFxcY2Fubm90IGJlIHVzZWQgb3V0c2lkZSBvZiB0aGUgY29pbiBjb250cmFjdC5cXFwiXFxuXFxuICAgIEBtb2RlbCBbIChwcm9wZXJ0eSAodmFsaWQtYWNjb3VudCBhY2NvdW50KSlcXG4gICAgICAgICAgICAgKHByb3BlcnR5ICg-IGFtb3VudCAwLjApKVxcbiAgICAgICAgICAgXVxcblxcbiAgICAodmFsaWRhdGUtYWNjb3VudCBhY2NvdW50KVxcbiAgICAoZW5mb3JjZS11bml0IGFtb3VudClcXG5cXG4gICAgKHJlcXVpcmUtY2FwYWJpbGl0eSAoQ09JTkJBU0UpKVxcbiAgICAoZW1pdC1ldmVudCAoVFJBTlNGRVIgXFxcIlxcXCIgYWNjb3VudCBhbW91bnQpKSA7djNcXG4gICAgKHdpdGgtY2FwYWJpbGl0eSAoQ1JFRElUIGFjY291bnQpXFxuICAgICAgKGNyZWRpdCBhY2NvdW50IGFjY291bnQtZ3VhcmQgYW1vdW50KSlcXG4gICAgKVxcblxcbiAgKGRlZnVuIHJlbWVkaWF0ZTpzdHJpbmcgKGFjY291bnQ6c3RyaW5nIGFtb3VudDpkZWNpbWFsKVxcbiAgICBAZG9jIFxcXCJBbGxvd3MgZm9yIHJlbWVkaWF0aW9uIHRyYW5zYWN0aW9ucy4gVGhpcyBmdW5jdGlvbiBcXFxcXFxuICAgICAgICAgXFxcXGlzIHByb3RlY3RlZCBieSB0aGUgUkVNRURJQVRFIGNhcGFiaWxpdHlcXFwiXFxuICAgIEBtb2RlbCBbIChwcm9wZXJ0eSAodmFsaWQtYWNjb3VudCBhY2NvdW50KSlcXG4gICAgICAgICAgICAgKHByb3BlcnR5ICg-IGFtb3VudCAwLjApKVxcbiAgICAgICAgICAgXVxcblxcbiAgICAodmFsaWRhdGUtYWNjb3VudCBhY2NvdW50KVxcblxcbiAgICAoZW5mb3JjZSAoPiBhbW91bnQgMC4wKVxcbiAgICAgIFxcXCJSZW1lZGlhdGlvbiBhbW91bnQgbXVzdCBiZSBwb3NpdGl2ZVxcXCIpXFxuXFxuICAgIChlbmZvcmNlLXVuaXQgYW1vdW50KVxcblxcbiAgICAocmVxdWlyZS1jYXBhYmlsaXR5IChSRU1FRElBVEUpKVxcbiAgICAoZW1pdC1ldmVudCAoVFJBTlNGRVIgXFxcIlxcXCIgYWNjb3VudCBhbW91bnQpKSA7djNcXG4gICAgKHdpdGgtcmVhZCBjb2luLXRhYmxlIGFjY291bnRcXG4gICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6PSBiYWxhbmNlIH1cXG5cXG4gICAgICAoZW5mb3JjZSAoPD0gYW1vdW50IGJhbGFuY2UpIFxcXCJJbnN1ZmZpY2llbnQgZnVuZHNcXFwiKVxcblxcbiAgICAgICh1cGRhdGUgY29pbi10YWJsZSBhY2NvdW50XFxuICAgICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6ICgtIGJhbGFuY2UgYW1vdW50KSB9XFxuICAgICAgICApKVxcbiAgICApXFxuXFxuICAoZGVmcGFjdCBmdW5kLXR4IChzZW5kZXI6c3RyaW5nIG1pbmVyOnN0cmluZyBtaW5lci1ndWFyZDpndWFyZCB0b3RhbDpkZWNpbWFsKVxcbiAgICBAZG9jIFxcXCInZnVuZC10eCcgaXMgYSBzcGVjaWFsIHBhY3QgdG8gZnVuZCBhIHRyYW5zYWN0aW9uIGluIHR3byBzdGVwcywgICAgIFxcXFxcXG4gICAgXFxcXHdpdGggdGhlIGFjdHVhbCB0cmFuc2FjdGlvbiB0cmFuc3BpcmluZyBpbiB0aGUgbWlkZGxlOiAgICAgICAgICAgICAgICAgICBcXFxcXFxuICAgIFxcXFwgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXFxcXFxcbiAgICBcXFxcICAxKSBBIGJ1eWluZyBwaGFzZSwgZGViaXRpbmcgdGhlIHNlbmRlciBmb3IgdG90YWwgZ2FzIGFuZCBmZWUsIHlpZWxkaW5nIFxcXFxcXG4gICAgXFxcXCAgICAgVFhfTUFYX0NIQVJHRS4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBcXFxcXFxuICAgIFxcXFwgIDIpIEEgc2V0dGxlbWVudCBwaGFzZSwgcmVzdW1pbmcgVFhfTUFYX0NIQVJHRSwgYW5kIGFsbG9jYXRpbmcgdG8gdGhlICAgXFxcXFxcbiAgICBcXFxcICAgICBjb2luYmFzZSBhY2NvdW50IGZvciB1c2VkIGdhcyBhbmQgZmVlLCBhbmQgc2VuZGVyIGFjY291bnQgZm9yIGJhbC0gIFxcXFxcXG4gICAgXFxcXCAgICAgYW5jZSAodW51c2VkIGdhcywgaWYgYW55KS5cXFwiXFxuXFxuICAgIEBtb2RlbCBbIChwcm9wZXJ0eSAoPiB0b3RhbCAwLjApKVxcbiAgICAgICAgICAgICAocHJvcGVydHkgKHZhbGlkLWFjY291bnQgc2VuZGVyKSlcXG4gICAgICAgICAgICAgKHByb3BlcnR5ICh2YWxpZC1hY2NvdW50IG1pbmVyKSlcXG4gICAgICAgICAgICAgOyhwcm9wZXJ0eSBjb25zZXJ2ZXMtbWFzcykgbm90IHN1cHBvcnRlZCB5ZXRcXG4gICAgICAgICAgIF1cXG5cXG4gICAgKHN0ZXAgKGJ1eS1nYXMgc2VuZGVyIHRvdGFsKSlcXG4gICAgKHN0ZXAgKHJlZGVlbS1nYXMgbWluZXIgbWluZXItZ3VhcmQgc2VuZGVyIHRvdGFsKSlcXG4gICAgKVxcblxcbiAgKGRlZnVuIGRlYml0OnN0cmluZyAoYWNjb3VudDpzdHJpbmcgYW1vdW50OmRlY2ltYWwpXFxuICAgIEBkb2MgXFxcIkRlYml0IEFNT1VOVCBmcm9tIEFDQ09VTlQgYmFsYW5jZVxcXCJcXG5cXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5ICg-IGFtb3VudCAwLjApKVxcbiAgICAgICAgICAgICAocHJvcGVydHkgKHZhbGlkLWFjY291bnQgYWNjb3VudCkpXFxuICAgICAgICAgICBdXFxuXFxuICAgICh2YWxpZGF0ZS1hY2NvdW50IGFjY291bnQpXFxuXFxuICAgIChlbmZvcmNlICg-IGFtb3VudCAwLjApXFxuICAgICAgXFxcImRlYml0IGFtb3VudCBtdXN0IGJlIHBvc2l0aXZlXFxcIilcXG5cXG4gICAgKGVuZm9yY2UtdW5pdCBhbW91bnQpXFxuXFxuICAgIChyZXF1aXJlLWNhcGFiaWxpdHkgKERFQklUIGFjY291bnQpKVxcbiAgICAod2l0aC1yZWFkIGNvaW4tdGFibGUgYWNjb3VudFxcbiAgICAgIHsgXFxcImJhbGFuY2VcXFwiIDo9IGJhbGFuY2UgfVxcblxcbiAgICAgIChlbmZvcmNlICg8PSBhbW91bnQgYmFsYW5jZSkgXFxcIkluc3VmZmljaWVudCBmdW5kc1xcXCIpXFxuXFxuICAgICAgKHVwZGF0ZSBjb2luLXRhYmxlIGFjY291bnRcXG4gICAgICAgIHsgXFxcImJhbGFuY2VcXFwiIDogKC0gYmFsYW5jZSBhbW91bnQpIH1cXG4gICAgICAgICkpXFxuICAgIClcXG5cXG5cXG4gIChkZWZ1biBjcmVkaXQ6c3RyaW5nIChhY2NvdW50OnN0cmluZyBndWFyZDpndWFyZCBhbW91bnQ6ZGVjaW1hbClcXG4gICAgQGRvYyBcXFwiQ3JlZGl0IEFNT1VOVCB0byBBQ0NPVU5UIGJhbGFuY2VcXFwiXFxuXFxuICAgIEBtb2RlbCBbIChwcm9wZXJ0eSAoPiBhbW91bnQgMC4wKSlcXG4gICAgICAgICAgICAgKHByb3BlcnR5ICh2YWxpZC1hY2NvdW50IGFjY291bnQpKVxcbiAgICAgICAgICAgXVxcblxcbiAgICAodmFsaWRhdGUtYWNjb3VudCBhY2NvdW50KVxcblxcbiAgICAoZW5mb3JjZSAoPiBhbW91bnQgMC4wKSBcXFwiY3JlZGl0IGFtb3VudCBtdXN0IGJlIHBvc2l0aXZlXFxcIilcXG4gICAgKGVuZm9yY2UtdW5pdCBhbW91bnQpXFxuXFxuICAgIChyZXF1aXJlLWNhcGFiaWxpdHkgKENSRURJVCBhY2NvdW50KSlcXG4gICAgKHdpdGgtZGVmYXVsdC1yZWFkIGNvaW4tdGFibGUgYWNjb3VudFxcbiAgICAgIHsgXFxcImJhbGFuY2VcXFwiIDogLTEuMCwgXFxcImd1YXJkXFxcIiA6IGd1YXJkIH1cXG4gICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6PSBiYWxhbmNlLCBcXFwiZ3VhcmRcXFwiIDo9IHJldGcgfVxcbiAgICAgIDsgd2UgZG9uJ3Qgd2FudCB0byBvdmVyd3JpdGUgYW4gZXhpc3RpbmcgZ3VhcmQgd2l0aCB0aGUgdXNlci1zdXBwbGllZCBvbmVcXG4gICAgICAoZW5mb3JjZSAoPSByZXRnIGd1YXJkKVxcbiAgICAgICAgXFxcImFjY291bnQgZ3VhcmRzIGRvIG5vdCBtYXRjaFxcXCIpXFxuXFxuICAgICAgKGxldCAoKGlzLW5ld1xcbiAgICAgICAgICAgICAoaWYgKD0gYmFsYW5jZSAtMS4wKVxcbiAgICAgICAgICAgICAgICAgKGVuZm9yY2UtY2hhaW53ZWItYWNjb3VudCBhY2NvdW50IGd1YXJkKVxcbiAgICAgICAgICAgICAgIGZhbHNlKSkpXFxuXFxuICAgICAgICAod3JpdGUgY29pbi10YWJsZSBhY2NvdW50XFxuICAgICAgICAgIHsgXFxcImJhbGFuY2VcXFwiIDogKGlmIGlzLW5ldyBhbW91bnQgKCsgYmFsYW5jZSBhbW91bnQpKVxcbiAgICAgICAgICAsIFxcXCJndWFyZFxcXCIgICA6IHJldGdcXG4gICAgICAgICAgfSkpXFxuICAgICAgKSlcXG5cXG4gIChkZWZ1biBlbmZvcmNlLWNoYWlud2ViLWFjY291bnQ6Ym9vbCAoYWNjb3VudDpzdHJpbmcgZ3VhcmQ6Z3VhcmQpXFxuICAgIEBkb2MgXFxcIkVuZm9yY2UgY2hhaW53ZWIgc2luZ2xlLWFjY291bnQgbmFtaW5nIHByb3RvY29sXFxcIlxcbiAgICAoaWYgKGFuZCAoPSBcXFwiazpcXFwiICh0YWtlIDIgYWNjb3VudCkpXFxuICAgICAgICAgICAgICg9IDY2IChsZW5ndGggYWNjb3VudCkpKVxcbiAgICAgIChlbmZvcmNlICg9IChmb3JtYXQgXFxcInt9XFxcIiBbZ3VhcmRdKVxcbiAgICAgICAgICAgICAgICAgIChmb3JtYXQgXFxcIktleVNldCB7a2V5czogW3t9XSxwcmVkOiBrZXlzLWFsbH1cXFwiIFsoZHJvcCAyIGFjY291bnQpXSkpXFxuICAgICAgICAgICAgICAgXFxcIlNpbmdsZS1rZXkgYWNjb3VudCBuYW1lIHZpb2xhdGlvblxcXCIpXFxuICAgICAgdHJ1ZSkpXFxuXFxuXFxuICAoZGVmc2NoZW1hIGNyb3NzY2hhaW4tc2NoZW1hXFxuICAgIEBkb2MgXFxcIlNjaGVtYSBmb3IgeWllbGRlZCB2YWx1ZSBpbiBjcm9zcy1jaGFpbiB0cmFuc2ZlcnNcXFwiXFxuICAgIHJlY2VpdmVyOnN0cmluZ1xcbiAgICByZWNlaXZlci1ndWFyZDpndWFyZFxcbiAgICBhbW91bnQ6ZGVjaW1hbClcXG5cXG4gIChkZWZwYWN0IHRyYW5zZmVyLWNyb3NzY2hhaW46c3RyaW5nXFxuICAgICggc2VuZGVyOnN0cmluZ1xcbiAgICAgIHJlY2VpdmVyOnN0cmluZ1xcbiAgICAgIHJlY2VpdmVyLWd1YXJkOmd1YXJkXFxuICAgICAgdGFyZ2V0LWNoYWluOnN0cmluZ1xcbiAgICAgIGFtb3VudDpkZWNpbWFsIClcXG5cXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5ICg-IGFtb3VudCAwLjApKVxcbiAgICAgICAgICAgICAocHJvcGVydHkgKHZhbGlkLWFjY291bnQgc2VuZGVyKSlcXG4gICAgICAgICAgICAgKHByb3BlcnR5ICh2YWxpZC1hY2NvdW50IHJlY2VpdmVyKSlcXG4gICAgICAgICAgIF1cXG5cXG4gICAgKHN0ZXBcXG4gICAgICAod2l0aC1jYXBhYmlsaXR5IChERUJJVCBzZW5kZXIpXFxuXFxuICAgICAgICAodmFsaWRhdGUtYWNjb3VudCBzZW5kZXIpXFxuICAgICAgICAodmFsaWRhdGUtYWNjb3VudCByZWNlaXZlcilcXG5cXG4gICAgICAgIChlbmZvcmNlICghPSBcXFwiXFxcIiB0YXJnZXQtY2hhaW4pIFxcXCJlbXB0eSB0YXJnZXQtY2hhaW5cXFwiKVxcbiAgICAgICAgKGVuZm9yY2UgKCE9IChhdCAnY2hhaW4taWQgKGNoYWluLWRhdGEpKSB0YXJnZXQtY2hhaW4pXFxuICAgICAgICAgIFxcXCJjYW5ub3QgcnVuIGNyb3NzLWNoYWluIHRyYW5zZmVycyB0byB0aGUgc2FtZSBjaGFpblxcXCIpXFxuXFxuICAgICAgICAoZW5mb3JjZSAoPiBhbW91bnQgMC4wKVxcbiAgICAgICAgICBcXFwidHJhbnNmZXIgcXVhbnRpdHkgbXVzdCBiZSBwb3NpdGl2ZVxcXCIpXFxuXFxuICAgICAgICAoZW5mb3JjZS11bml0IGFtb3VudClcXG5cXG4gICAgICAgIDs7IHN0ZXAgMSAtIGRlYml0IGRlbGV0ZS1hY2NvdW50IG9uIGN1cnJlbnQgY2hhaW5cXG4gICAgICAgIChkZWJpdCBzZW5kZXIgYW1vdW50KVxcblxcbiAgICAgICAgKGVtaXQtZXZlbnQgKFRSQU5TRkVSIHNlbmRlciBcXFwiXFxcIiBhbW91bnQpKVxcblxcbiAgICAgICAgKGxldFxcbiAgICAgICAgICAoKGNyb3NzY2hhaW4tZGV0YWlsczpvYmplY3R7Y3Jvc3NjaGFpbi1zY2hlbWF9XFxuICAgICAgICAgICAgeyBcXFwicmVjZWl2ZXJcXFwiIDogcmVjZWl2ZXJcXG4gICAgICAgICAgICAsIFxcXCJyZWNlaXZlci1ndWFyZFxcXCIgOiByZWNlaXZlci1ndWFyZFxcbiAgICAgICAgICAgICwgXFxcImFtb3VudFxcXCIgOiBhbW91bnRcXG4gICAgICAgICAgICB9KSlcXG4gICAgICAgICAgKHlpZWxkIGNyb3NzY2hhaW4tZGV0YWlscyB0YXJnZXQtY2hhaW4pXFxuICAgICAgICAgICkpKVxcblxcbiAgICAoc3RlcFxcbiAgICAgIChyZXN1bWVcXG4gICAgICAgIHsgXFxcInJlY2VpdmVyXFxcIiA6PSByZWNlaXZlclxcbiAgICAgICAgLCBcXFwicmVjZWl2ZXItZ3VhcmRcXFwiIDo9IHJlY2VpdmVyLWd1YXJkXFxuICAgICAgICAsIFxcXCJhbW91bnRcXFwiIDo9IGFtb3VudFxcbiAgICAgICAgfVxcbiAgICAgICAgKGVtaXQtZXZlbnQgKFRSQU5TRkVSIFxcXCJcXFwiIHJlY2VpdmVyIGFtb3VudCkpXFxuICAgICAgICA7OyBzdGVwIDIgLSBjcmVkaXQgY3JlYXRlIGFjY291bnQgb24gdGFyZ2V0IGNoYWluXFxuICAgICAgICAod2l0aC1jYXBhYmlsaXR5IChDUkVESVQgcmVjZWl2ZXIpXFxuICAgICAgICAgIChjcmVkaXQgcmVjZWl2ZXIgcmVjZWl2ZXItZ3VhcmQgYW1vdW50KSlcXG4gICAgICAgICkpXFxuICAgIClcXG5cXG5cXG4gIDsgLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS1cXG4gIDsgQ29pbiBhbGxvY2F0aW9uc1xcblxcbiAgKGRlZnNjaGVtYSBhbGxvY2F0aW9uLXNjaGVtYVxcbiAgICBAZG9jIFxcXCJHZW5lc2lzIGFsbG9jYXRpb24gcmVnaXN0cnlcXFwiXFxuICAgIDtAbW9kZWwgWyAoaW52YXJpYW50ICg-PSBiYWxhbmNlIDAuMCkpIF1cXG5cXG4gICAgYmFsYW5jZTpkZWNpbWFsXFxuICAgIGRhdGU6dGltZVxcbiAgICBndWFyZDpndWFyZFxcbiAgICByZWRlZW1lZDpib29sKVxcblxcbiAgKGRlZnRhYmxlIGFsbG9jYXRpb24tdGFibGU6e2FsbG9jYXRpb24tc2NoZW1hfSlcXG5cXG4gIChkZWZ1biBjcmVhdGUtYWxsb2NhdGlvbi1hY2NvdW50XFxuICAgICggYWNjb3VudDpzdHJpbmdcXG4gICAgICBkYXRlOnRpbWVcXG4gICAgICBrZXlzZXQtcmVmOnN0cmluZ1xcbiAgICAgIGFtb3VudDpkZWNpbWFsXFxuICAgIClcXG5cXG4gICAgQGRvYyBcXFwiQWRkIGFuIGVudHJ5IHRvIHRoZSBjb2luIGFsbG9jYXRpb24gdGFibGUuIFRoaXMgZnVuY3Rpb24gXFxcXFxcbiAgICAgICAgIFxcXFxhbHNvIGNyZWF0ZXMgYSBjb3JyZXNwb25kaW5nIGVtcHR5IGNvaW4gY29udHJhY3QgYWNjb3VudCBcXFxcXFxuICAgICAgICAgXFxcXG9mIHRoZSBzYW1lIG5hbWUgYW5kIGd1YXJkLiBSZXF1aXJlcyBHRU5FU0lTIGNhcGFiaWxpdHkuIFxcXCJcXG5cXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5ICh2YWxpZC1hY2NvdW50IGFjY291bnQpKSBdXFxuXFxuICAgIChyZXF1aXJlLWNhcGFiaWxpdHkgKEdFTkVTSVMpKVxcblxcbiAgICAodmFsaWRhdGUtYWNjb3VudCBhY2NvdW50KVxcbiAgICAoZW5mb3JjZSAoPj0gYW1vdW50IDAuMClcXG4gICAgICBcXFwiYWxsb2NhdGlvbiBhbW91bnQgbXVzdCBiZSBub24tbmVnYXRpdmVcXFwiKVxcblxcbiAgICAoZW5mb3JjZS11bml0IGFtb3VudClcXG5cXG4gICAgKGxldFxcbiAgICAgICgoZ3VhcmQ6Z3VhcmQgKGtleXNldC1yZWYtZ3VhcmQga2V5c2V0LXJlZikpKVxcblxcbiAgICAgIChjcmVhdGUtYWNjb3VudCBhY2NvdW50IGd1YXJkKVxcblxcbiAgICAgIChpbnNlcnQgYWxsb2NhdGlvbi10YWJsZSBhY2NvdW50XFxuICAgICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6IGFtb3VudFxcbiAgICAgICAgLCBcXFwiZGF0ZVxcXCIgOiBkYXRlXFxuICAgICAgICAsIFxcXCJndWFyZFxcXCIgOiBndWFyZFxcbiAgICAgICAgLCBcXFwicmVkZWVtZWRcXFwiIDogZmFsc2VcXG4gICAgICAgIH0pKSlcXG5cXG4gIChkZWZ1biByZWxlYXNlLWFsbG9jYXRpb25cXG4gICAgKCBhY2NvdW50OnN0cmluZyApXFxuXFxuICAgIEBkb2MgXFxcIlJlbGVhc2UgZnVuZHMgYXNzb2NpYXRlZCB3aXRoIGFsbG9jYXRpb24gQUNDT1VOVCBpbnRvIG1haW4gbGVkZ2VyLiAgIFxcXFxcXG4gICAgICAgICBcXFxcQUNDT1VOVCBtdXN0IGFscmVhZHkgZXhpc3QgaW4gbWFpbiBsZWRnZXIuIEFsbG9jYXRpb24gaXMgZGVhY3RpdmF0ZWQgXFxcXFxcbiAgICAgICAgIFxcXFxhZnRlciByZWxlYXNlLlxcXCJcXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5ICh2YWxpZC1hY2NvdW50IGFjY291bnQpKSBdXFxuXFxuICAgICh2YWxpZGF0ZS1hY2NvdW50IGFjY291bnQpXFxuXFxuICAgICh3aXRoLXJlYWQgYWxsb2NhdGlvbi10YWJsZSBhY2NvdW50XFxuICAgICAgeyBcXFwiYmFsYW5jZVxcXCIgOj0gYmFsYW5jZVxcbiAgICAgICwgXFxcImRhdGVcXFwiIDo9IHJlbGVhc2UtdGltZVxcbiAgICAgICwgXFxcInJlZGVlbWVkXFxcIiA6PSByZWRlZW1lZFxcbiAgICAgICwgXFxcImd1YXJkXFxcIiA6PSBndWFyZFxcbiAgICAgIH1cXG5cXG4gICAgICAobGV0ICgoY3Vyci10aW1lOnRpbWUgKGF0ICdibG9jay10aW1lIChjaGFpbi1kYXRhKSkpKVxcblxcbiAgICAgICAgKGVuZm9yY2UgKG5vdCByZWRlZW1lZClcXG4gICAgICAgICAgXFxcImFsbG9jYXRpb24gZnVuZHMgaGF2ZSBhbHJlYWR5IGJlZW4gcmVkZWVtZWRcXFwiKVxcblxcbiAgICAgICAgKGVuZm9yY2VcXG4gICAgICAgICAgKD49IGN1cnItdGltZSByZWxlYXNlLXRpbWUpXFxuICAgICAgICAgIChmb3JtYXQgXFxcImZ1bmRzIGxvY2tlZCB1bnRpbCB7fS4gY3VycmVudCB0aW1lOiB7fVxcXCIgW3JlbGVhc2UtdGltZSBjdXJyLXRpbWVdKSlcXG5cXG4gICAgICAgICh3aXRoLWNhcGFiaWxpdHkgKFJFTEVBU0VfQUxMT0NBVElPTiBhY2NvdW50IGJhbGFuY2UpXFxuXFxuICAgICAgICAoZW5mb3JjZS1ndWFyZCBndWFyZClcXG5cXG4gICAgICAgICh3aXRoLWNhcGFiaWxpdHkgKENSRURJVCBhY2NvdW50KVxcbiAgICAgICAgICAoZW1pdC1ldmVudCAoVFJBTlNGRVIgXFxcIlxcXCIgYWNjb3VudCBiYWxhbmNlKSlcXG4gICAgICAgICAgKGNyZWRpdCBhY2NvdW50IGd1YXJkIGJhbGFuY2UpXFxuXFxuICAgICAgICAgICh1cGRhdGUgYWxsb2NhdGlvbi10YWJsZSBhY2NvdW50XFxuICAgICAgICAgICAgeyBcXFwicmVkZWVtZWRcXFwiIDogdHJ1ZVxcbiAgICAgICAgICAgICwgXFxcImJhbGFuY2VcXFwiIDogMC4wXFxuICAgICAgICAgICAgfSlcXG5cXG4gICAgICAgICAgXFxcIkFsbG9jYXRpb24gc3VjY2Vzc2Z1bGx5IHJlbGVhc2VkIHRvIG1haW4gbGVkZ2VyXFxcIikpXFxuICAgICkpKVxcblxcbilcXG5cIn19LFwic2lnbmVyc1wiOltdLFwibWV0YVwiOntcImNyZWF0aW9uVGltZVwiOjAsXCJ0dGxcIjoxNzI4MDAsXCJnYXNMaW1pdFwiOjAsXCJjaGFpbklkXCI6XCJcIixcImdhc1ByaWNlXCI6MCxcInNlbmRlclwiOlwiXCJ9LFwibm9uY2VcIjpcImNvaW4tY29udHJhY3QtdjNcIn0ifQ"
    ]
