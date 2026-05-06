-- SPDX-License-Identifier: Apache-2.0
-- | Contact/identity management
--
-- See: doc/spec/chat.md
module UmbraVox.Chat.Contacts
  ( ContactList
  , emptyContacts
  ) where

-- | A user's contact list.
data ContactList = ContactList
  deriving (Show)

-- | An empty contact list.
emptyContacts :: ContactList
emptyContacts = ContactList
