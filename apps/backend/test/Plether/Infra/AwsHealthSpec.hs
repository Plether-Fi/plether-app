module Plether.Infra.AwsHealthSpec (spec) where

import Data.Aeson (object, (.=))
import Plether.Infra.AwsHealth
  ( AwsEc2Status (..)
  , AwsEcsServiceStatus (..)
  , AwsRdsStatus (..)
  , chunkList
  , normalizeServiceKey
  , parseEc2StatusXml
  , parseEcsServicesJson
  , parseRdsStatusXml
  )
import Test.Hspec

spec :: Spec
spec = describe "Plether.Infra.AwsHealth" $ do
  it "parses ECS DescribeServices JSON cleanly without exposing ARNs or task definitions" $ do
    let payload =
          object
            [ "services"
                .= [ object
                       [ "serviceName" .= ("plether-api" :: String)
                       , "serviceArn" .= ("arn:aws:ecs:us-east-1:123456789012:service/cluster/plether-api" :: String)
                       , "status" .= ("ACTIVE" :: String)
                       , "desiredCount" .= (2 :: Int)
                       , "runningCount" .= (2 :: Int)
                       , "pendingCount" .= (0 :: Int)
                       , "deployments" .= [object ["rolloutState" .= ("COMPLETED" :: String)]]
                       ]
                   , object
                       [ "serviceName" .= ("plether-sepolia-rpc-gateway" :: String)
                       , "status" .= ("ACTIVE" :: String)
                       , "desiredCount" .= (2 :: Int)
                       , "runningCount" .= (1 :: Int)
                       , "pendingCount" .= (1 :: Int)
                       ]
                   ]
            ]
    parseEcsServicesJson payload
      `shouldBe` [ AwsEcsServiceStatus "api" "ACTIVE" 2 2 0 (Just "COMPLETED")
                 , AwsEcsServiceStatus "rpc_gateway" "ACTIVE" 2 1 1 Nothing
                 ]

  it "parses RDS DescribeDBInstances XML without touching hostnames or ARNs" $ do
    let xml =
          "<DescribeDBInstancesResponse><DescribeDBInstancesResult><DBInstances><DBInstance>\
          \<DBInstanceStatus>available</DBInstanceStatus>\
          \<EngineVersion>16.4</EngineVersion>\
          \<MultiAZ>false</MultiAZ>\
          \<StorageEncrypted>true</StorageEncrypted>\
          \<AllocatedStorage>100</AllocatedStorage>\
          \</DBInstance></DBInstances></DescribeDBInstancesResult></DescribeDBInstancesResponse>"
    parseRdsStatusXml xml
      `shouldBe` Just (AwsRdsStatus "available" (Just "16.4") (Just False) (Just True) (Just 100))
    parseRdsStatusXml "<empty/>" `shouldBe` Nothing

  it "parses EC2 DescribeInstanceStatus XML cleanly" $ do
    let xml =
          "<DescribeInstanceStatusResponse><instanceStatusSet><item>\
          \<instanceState><code>16</code><name>running</name></instanceState>\
          \<systemStatus><status>ok</status></systemStatus>\
          \<instanceStatus><status>ok</status></instanceStatus>\
          \</item></instanceStatusSet></DescribeInstanceStatusResponse>"
    parseEc2StatusXml xml
      `shouldBe` Just (AwsEc2Status "running" (Just "ok") (Just "ok"))

  it "normalizes service names and chunks ECS service lists into batches of 10" $ do
    normalizeServiceKey "plether-keeper" `shouldBe` "keeper"
    normalizeServiceKey "plether-position-protection-worker" `shouldBe` "protection_worker"
    normalizeServiceKey "plether-mainnet-rpc-gateway" `shouldBe` "rpc_gateway"
    length (chunkList 10 ([1 .. 23] :: [Int])) `shouldBe` 3
