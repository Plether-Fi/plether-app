import { readFileSync } from 'node:fs'
import path from 'node:path'
import { describe, expect, it } from 'vitest'
import publicManifest from '../../../public/perps-aa-manifest.json'
import { PERPS_ARBITRUM_SEPOLIA } from '../perpsAddresses'

interface PinnedContract {
  address: string
  runtimeCodeHash: string
}

interface PinnedRelease {
  schemaVersion: number
  network: { chainId: number }
  release: {
    sourceRepository: string
    sourceArtifact: string
    artifactCommit: string
    artifactBlobSha: string
    sourceCommit: string
    deploymentBlock: number
  }
  contracts: Record<string, PinnedContract>
}

const repositoryRoot = path.resolve(process.cwd(), '../..')
const pinnedRelease = JSON.parse(readFileSync(
  path.join(repositoryRoot, 'config/perps/arbitrum-sepolia-v2.json'),
  'utf8'
)) as PinnedRelease

describe('pinned bounded-V2 Sepolia release', () => {
  it('retains authoritative plether-core provenance and runtime hashes', () => {
    expect(pinnedRelease).toMatchObject({
      schemaVersion: 2,
      network: { chainId: 421614 },
      release: {
        sourceRepository: 'Plether-Fi/plether-core',
        sourceArtifact: 'deployments/releases/2026-09-05-perps-arbitrum-sepolia/manifest.json',
        artifactCommit: '999691615ec2fb7443c1920547e148633ec9aca2',
        artifactBlobSha: '9127fb9a6b3d7bcf9875379a2a37e2ff28598c26',
        sourceCommit: 'c3f60f58bcd5dc1b85a28739a5de7ec4a2ee114c',
        deploymentBlock: 305627052,
      },
    })
    for (const contract of Object.values(pinnedRelease.contracts)) {
      expect(contract.runtimeCodeHash).toMatch(/^0x[0-9a-f]{64}$/)
    }
  })

  it('keeps the frontend registry and public manifest on the same release', () => {
    expect(publicManifest).toMatchObject({
      version: 'perps-aa-arbitrum-sepolia-20260905-v2',
      chainId: pinnedRelease.network.chainId,
      orderRouter: pinnedRelease.contracts.orderRouter.address,
      orderLifecycleBook: pinnedRelease.contracts.orderLifecycleBook.address,
      policyEvaluator: pinnedRelease.contracts.cfdOrderPolicyEvaluator.address,
      sponsorshipEnabled: true,
    })
    expect(PERPS_ARBITRUM_SEPOLIA).toMatchObject({
      usdc: pinnedRelease.contracts.mockUsdc.address,
      marginClearinghouse: pinnedRelease.contracts.marginClearinghouse.address,
      cfdEngine: pinnedRelease.contracts.cfdEngine.address,
      housePool: pinnedRelease.contracts.housePool.address,
      orderRouter: pinnedRelease.contracts.orderRouter.address,
      orderLifecycleBook: pinnedRelease.contracts.orderLifecycleBook.address,
      policyEvaluator: pinnedRelease.contracts.cfdOrderPolicyEvaluator.address,
      positionProtectionBook: pinnedRelease.contracts.positionProtectionBook.address,
      perpsPublicLens: pinnedRelease.contracts.perpsPublicLens.address,
    })
  })

  it('keeps Terraform and deployment workflows fail-closed on V2', () => {
    const terraformExample = readFileSync(
      path.join(repositoryRoot, 'infra/terraform/terraform.tfvars.sepolia.example'),
      'utf8'
    )
    const terraformEcs = readFileSync(
      path.join(repositoryRoot, 'infra/terraform/ecs.tf'),
      'utf8'
    )
    const backendWorkflow = readFileSync(
      path.join(repositoryRoot, '.github/workflows/deploy-backend.yml'),
      'utf8'
    )
    const frontendWorkflow = readFileSync(
      path.join(repositoryRoot, '.github/workflows/deploy-frontend.yml'),
      'utf8'
    )
    const backendRelease = readFileSync(
      path.join(repositoryRoot, 'apps/backend/src/Plether/Perps/Release.hs'),
      'utf8'
    )

    const terraformBindings = [
      ['perps_usdc', pinnedRelease.contracts.mockUsdc.address],
      ['perps_order_router', pinnedRelease.contracts.orderRouter.address],
      ['perps_order_lifecycle_book', pinnedRelease.contracts.orderLifecycleBook.address],
      ['perps_cfd_engine', pinnedRelease.contracts.cfdEngine.address],
      ['perps_margin_clearinghouse', pinnedRelease.contracts.marginClearinghouse.address],
      ['perps_house_pool', pinnedRelease.contracts.housePool.address],
    ] as const
    for (const [field, address] of terraformBindings) {
      expect(terraformExample).toMatch(
        new RegExp(`${field}\\s*=\\s*"${address}"`, 'i')
      )
      expect(backendWorkflow.toLowerCase()).toContain(address.toLowerCase())
    }
    expect(terraformExample).toContain(
      `perps_indexer_start_block      = "${pinnedRelease.release.deploymentBlock}"`
    )
    expect(backendWorkflow).not.toContain(
      'upsert_env("AA_SPONSORSHIP_ENABLED"; "true")'
    )
    const apiTaskDefinition = terraformEcs.slice(
      terraformEcs.indexOf('resource "aws_ecs_task_definition" "api"'),
      terraformEcs.indexOf('resource "aws_ecs_service" "api"')
    )
    expect(apiTaskDefinition).toContain(
      '{ name = "PERPS_HOUSE_POOL", value = var.perps_house_pool }'
    )
    const insightsTaskDefinition = terraformEcs.slice(
      terraformEcs.indexOf('resource "aws_ecs_task_definition" "insights_worker"'),
      terraformEcs.indexOf('resource "aws_ecs_service" "insights_worker"')
    )
    const consolidatedWorkersTaskDefinition = terraformEcs.slice(
      terraformEcs.indexOf('resource "aws_ecs_task_definition" "workers"'),
      terraformEcs.indexOf('resource "aws_ecs_service" "workers"')
    )
    const perpsIndexerTaskDefinition = terraformEcs.slice(
      terraformEcs.indexOf('resource "aws_ecs_task_definition" "perps_indexer"'),
      terraformEcs.indexOf('resource "aws_ecs_service" "perps_indexer"')
    )
    const consolidatedPerpsIndexer = consolidatedWorkersTaskDefinition.slice(
      consolidatedWorkersTaskDefinition.indexOf('name             = "plether-perps-indexer"'),
      consolidatedWorkersTaskDefinition.indexOf('name             = "plether-insights-worker"')
    )
    for (const taskDefinition of [
      insightsTaskDefinition,
      consolidatedWorkersTaskDefinition,
      perpsIndexerTaskDefinition,
      consolidatedPerpsIndexer,
    ]) {
      expect(taskDefinition).toContain(
        '{ name = "PERPS_ORDER_LIFECYCLE_BOOK", value = var.perps_order_lifecycle_book }'
      )
      expect(taskDefinition).toContain(
        '{ name = "PERPS_HOUSE_POOL", value = var.perps_house_pool }'
      )
    }
    expect(backendWorkflow).toContain('"PERPS_HOUSE_POOL",')
    expect(backendRelease).toContain('Manifest.releaseAaManifestVersion')
    expect(backendRelease).toContain('Manifest.orderLifecycleBookAddress')
    expect(backendRelease).toContain('Manifest.cfdOrderPolicyEvaluatorCodeHash')
    expect(backendRelease).not.toMatch(/0x[0-9a-fA-F]{40,64}/)
    const backendManifest = readFileSync(
      path.join(repositoryRoot, 'apps/backend/src/Plether/Perps/Manifest.hs'),
      'utf8'
    )
    expect(backendManifest).toContain('manifestText ["contracts", "orderLifecycleBook", "address"]')
    expect(backendManifest).toContain('manifestText ["contracts", "cfdOrderPolicyEvaluator", "runtimeCodeHash"]')
    expect(pinnedRelease).toMatchObject({
      integration: { aaManifestVersion: publicManifest.version },
    })
    expect(frontendWorkflow).toContain('/api/aa/status')
    expect(frontendWorkflow).toContain('calldataPolicy == "bounded-v2"')
  })
})
