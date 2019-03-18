module ArangoDB.Documents (
  -- * Documents

  -- ** Read document
  getDocument,
  getDocumentById,

  -- ** Create document
  createDocument,

  -- ** Update document
  updateDocument,
  updateDocument_,

  -- *** Methods with extra parameters
  updateDocument',
  updateDocumentReturnOld',
  updateDocumentSilent',

  -- ** Drop document
  dropDocument,
  dropDocument_,

  -- *** Methods with extra parameters
  dropDocument',
  dropDocumentReturnOld',
  dropDocumentSilent',

) where

import           ArangoDB.Internal.Documents
import           ArangoDB.Types
import           ArangoDB.Utils.Client
import           Data.Aeson            (FromJSON, ToJSON)
import           Data.Aeson.WithField  (OnlyField (..))
import           Data.Proxy
import           Servant.API
import           ArangoDB.Types
import           ArangoDB.Utils.Client
import           Data.Aeson            (FromJSON, ToJSON)
import           Data.Aeson.WithField  (OnlyField (..))
import           Data.Proxy
import           Servant.API

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XFlexibleInstances
-- >>> :set -XDeriveGeneric
-- >>> :set -XDeriveAnyClass
-- >>> :set -XScopedTypeVariables
-- >>> import Data.Aeson
-- >>> import GHC.Generics
-- >>> import Data.Either (isRight, isLeft)
-- >>> import Data.Aeson.WithField  (OnlyField (..))
-- >>> import ArangoDB.Collections
-- >>> data Person = Person { firstname :: String, lastname :: String } deriving (Show, Generic, ToJSON, FromJSON)
-- >>> user1 = Person "Nick" "K."
-- >>> user2 = Person "gang" "w."
-- >>> collectionName = "Person" :: TypedCollectionName Person
-- >>> createCollectionResult = runDefault $ createCollection "Person"
-- >>> fmap isRight createCollectionResult
-- True

getDocument :: forall a. FromJSON a => (TypedCollectionName a) -> DocumentKey -> ArangoClientM (Document a)
getDocument = arangoClient (Proxy @(GetDocument a))

getDocumentById :: FromJSON a => DocumentId a -> ArangoClientM (Document a)
getDocumentById = uncurry getDocument . splitDocumentId

type DropDocument
  = "document"
 :> Capture "collection-name" CollectionName
 :> QueryParam "returnOld" Bool
 :> Capture "document-key" DocumentKey
 :> QueryParam "waitForSync" Bool
 :> QueryParam "silent" Bool
 :> Header "If-Match" DocumentRevision
 :> Delete '[JSON] DropDocumentResponse


dropDocument :: CollectionName
               -> ReturnOld
               -> DocumentKey
               -> WaitForSync
               -> Silent
               -> IfMatch
               -> ArangoClientM DropDocumentResponse

dropDocument = arangoClient (Proxy @DropDocument)

type DropDocumentReturnOld a
  = "document"
 :> Capture "collection-name" (TypedCollectionName a)
 :> QueryParam "returnOld" Bool
 :> Capture "document-key" DocumentKey
 :> QueryParam "waitForSync" Bool
 :> QueryParam "silent" Bool
 :> Header "If-Match" DocumentRevision
 :> ReqBody '[JSON] a
 :> Delete '[JSON] (OnlyField "old" (Document a))


dropDocumentReturnOld :: forall a. (ToJSON a, FromJSON a) =>
               (TypedCollectionName a)
               -> DocumentKey
               -> WaitForSync
               -> Silent
               -> IfMatch
               -> a
               -> ArangoClientM (Document a) 
dropDocumentReturnOld typedCollectionName docKey waitForSync silent ifMatch doc = unOnlyField <$> arangoClient (Proxy @(DropDocumentReturnOld a)) typedCollectionName (Just True) docKey waitForSync silent ifMatch doc

type UpdateDocument a
    = "document"
    :> Capture "collection-name" CollectionName
    :> QueryParam "returnOld" Bool
    :> Capture "document-key" DocumentKey
    :> QueryParam "waitForSync" Bool
    :> QueryParam "silent" Bool
    :> Header "If-Match" DocumentRevision
    :> ReqBody '[JSON] a
    :> Put '[JSON] UpdateDocumentResponse

updateDocument :: forall a. (ToJSON a, FromJSON a) =>
            CollectionName
            -> ReturnOld
            -> DocumentKey
            -> WaitForSync
            -> Silent
            -> IfMatch
            -> a
            -> ArangoClientM UpdateDocumentResponse
updateDocument = arangoClient (Proxy @(UpdateDocument a))


type UpdateDocumentReturnOld a
    = "document"
    :> Capture "collection-name" (TypedCollectionName a)
    :> QueryParam "returnOld" Bool
    :> Capture "document-key" DocumentKey
    :> QueryParam "waitForSync" Bool
    :> QueryParam "silent" Bool
    :> Header "If-Match" DocumentRevision
    :> ReqBody '[JSON] a
    :> Put '[JSON] (OnlyField "old" (Document a))

updateDocumentReturnOld :: forall a. (ToJSON a, FromJSON a) =>
            (TypedCollectionName a)
            -> DocumentKey
            -> WaitForSync
            -> Silent
            -> IfMatch
            -> a
            -> ArangoClientM (Document a)
updateDocumentReturnOld typedCollectionName docKey waitForSync silent ifMatch doc = unOnlyField <$> arangoClient (Proxy @(UpdateDocumentReturnOld a)) typedCollectionName (Just True) docKey waitForSync silent ifMatch doc

-- | Doctest for the functions above
-- >>> Right (Document docId1 docKey1 docRev1 _) <- runDefault $ createDocument collectionName (Just False) (Just False) (Just False) user1
-- >>> Right (person :: Document Person) <- runDefault $ getDocument collectionName docKey1
-- >>> documentValue person
-- Person {firstname = "Nick", lastname = "K."}
-- >>> failedDropResult = runDefault $ dropDocument collectionName (Just False)  (DocumentKey "key")  (Just False) (Just False) (Just (DocumentRevision "1")) user1
-- >>> fmap isLeft failedDropResult
-- True
-- >>> dropResult = runDefault $ dropDocument collectionName (Just False)  docKey1  (Just False) (Just False) (Just docRev1) user1
-- >>> fmap isRight dropResult
-- True
-- >>> Right (Document docId2 docKey2 docRev2 _) <- runDefault $ createDocument collectionName (Just False) (Just False) (Just False) user1
-- >>> Right (Document docId3 docKey3 docRev3 _) <- runDefault $ updateDocument collectionName (Just False)  docKey2  (Just False) (Just False) (Just docRev2) user2
-- >>> dropResult1 = runDefault $ dropDocument collectionName (Just False)  docKey3  (Just False) (Just False) (Just docRev3) user2
-- >>> fmap isRight dropResult1
<<<<<<< HEAD
-- True
-- >>> Right (Document docId4 docKey4 docRev4 _) <- runDefault $ createDocument collectionName (Just False) (Just False) (Just False) user1
-- >>> updateDocumentFullFunc = runDefault $ updateDocumentReturnOld collectionName  docKey4  (Just False) (Just False) (Just docRev4) user2
-- >>> Right (Document docId5 docKey5 docRev5 docValue1) <- updateDocumentFullFunc
-- >>> docValue1
-- Person {firstname = "Nick", lastname = "K."}
-- >>> Right (Document docId6 docKey6 docRev6 _) <- runDefault $ createDocument collectionName (Just False) (Just False) (Just False) user1
-- >>> Right (Document docId7 docKey7 docRev7 docValue3) <- runDefault $ dropDocumentReturnOld collectionName docKey6  (Just False) (Just False) (Just docRev6) user1
-- >>> docValue3
-- Person {firstname = "Nick", lastname = "K."}
-- >>> dropCollectionFunc = runDefault $ dropCollection "Person"
-- >>> fmap isRight dropCollectionFunc
-- True
=======
-- True
-- >>> Right (Document docId4 docKey4 docRev4 _) <- runDefault $ createDocument collectionName (Just False) (Just False) (Just False) user1
-- >>> updateDocumentFullFunc = runDefault $ updateDocumentReturnOld collectionName  docKey4  (Just False) (Just False) (Just docRev4) user2
-- >>> Right (Document docId5 docKey5 docRev5 docValue1) <- updateDocumentFullFunc
-- >>> docValue1
-- Person {firstname = "Nick", lastname = "K."}
-- >>> Right (Document docId6 docKey6 docRev6 _) <- runDefault $ createDocument collectionName (Just False) (Just False) (Just False) user1
-- >>> Right (Document docId7 docKey7 docRev7 docValue3) <- runDefault $ dropDocumentReturnOld collectionName docKey6  (Just False) (Just False) (Just docRev6) user1
-- >>> docValue3
-- Person {firstname = "Nick", lastname = "K."}
-- >>> dropCollectionFunc = runDefault $ dropCollection "Person"
-- >>> fmap isRight dropCollectionFunc
-- True
>>>>>>> 5d68989a158e94369bfba50708274cb23fd55542
