{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module DropboxSpec
  ( spec
  ) where

import Dropbox
import Data.Aeson.QQ
import Data.Aeson
import Test.Hspec

spec :: Spec
spec = do
  describe "Dropbox aeson aeson" $ do
    it "encodes list folder correctly" $
      encode defListFolderRequest `shouldBe` "{\"include_non_downloadable_files\":false,\"path\":\"\",\"include_mounted_folders\":true,\"include_deleted\":false,\"include_media_info\":false,\"include_has_explicit_shared_members\":false,\"recursive\":false}"
  describe "Example response" $ do
    it "Decodes correctly" $ fromJSON yyy `shouldBe` Success (ListFolderResponse
      [ Entry { eTag = File, eName = "Prime_Numbers.txt", ePathDisplay = "/Homework/math/Prime_Numbers.txt", eId = "id:a4ayc_80_OEAAAAAAAAAXw" }
      , Entry { eTag = Folder, eName = "math", ePathDisplay = "/Homework/math", eId = "id:a4ayc_80_OEAAAAAAAAAXz" }
      ])

yyy :: Value
yyy = [aesonQQ|
{
    "entries": [
        {
            ".tag": "file",
            "name": "Prime_Numbers.txt",
            "id": "id:a4ayc_80_OEAAAAAAAAAXw",
            "client_modified": "2015-05-12T15:50:38Z",
            "server_modified": "2015-05-12T15:50:38Z",
            "rev": "a1c10ce0dd78",
            "size": 7212,
            "path_lower": "/homework/math/prime_numbers.txt",
            "path_display": "/Homework/math/Prime_Numbers.txt",
            "sharing_info": {
                "read_only": true,
                "parent_shared_folder_id": "84528192421",
                "modified_by": "dbid:AAH4f99T0taONIb-OurWxbNQ6ywGRopQngc"
            },
            "is_downloadable": true,
            "property_groups": [
                {
                    "template_id": "ptid:1a5n2i6d3OYEAAAAAAAAAYa",
                    "fields": [
                        {
                            "name": "Security Policy",
                            "value": "Confidential"
                        }
                    ]
                }
            ],
            "has_explicit_shared_members": false,
            "content_hash": "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
            "file_lock_info": {
                "is_lockholder": true,
                "lockholder_name": "Imaginary User",
                "created": "2015-05-12T15:50:38Z"
            }
        },
        {
            ".tag": "folder",
            "name": "math",
            "id": "id:a4ayc_80_OEAAAAAAAAAXz",
            "path_lower": "/homework/math",
            "path_display": "/Homework/math",
            "sharing_info": {
                "read_only": false,
                "parent_shared_folder_id": "84528192421",
                "traverse_only": false,
                "no_access": false
            },
            "property_groups": [
                {
                    "template_id": "ptid:1a5n2i6d3OYEAAAAAAAAAYa",
                    "fields": [
                        {
                            "name": "Security Policy",
                            "value": "Confidential"
                        }
                    ]
                }
            ]
        }
    ],
    "cursor": "ZtkX9_EHj3x7PMkVuFIhwKYXEpwpLwyxp9vMKomUhllil9q7eWiAu",
    "has_more": false
}
|]
