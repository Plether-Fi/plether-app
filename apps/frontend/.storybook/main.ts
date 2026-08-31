import type { StorybookConfig } from '@storybook/react-vite';
import { mergeConfig } from 'vite';

const config: StorybookConfig = {
  "stories": [
    "../src/**/*.mdx",
    "../src/**/*.stories.@(js|jsx|mjs|ts|tsx)"
  ],
  "addons": [
    "@chromatic-com/storybook",
    "@storybook/addon-vitest",
    "@storybook/addon-a11y",
    "@storybook/addon-docs",
    "@storybook/addon-onboarding"
  ],
  "framework": "@storybook/react-vite",
  viteFinal: async (viteConfig) => mergeConfig(viteConfig, {
    resolve: {
      // Insights stories import production components from a sibling app. Force
      // both apps to share the same React and router contexts in Storybook.
      dedupe: ['react', 'react-dom', 'react-router', 'react-router-dom'],
    },
  }),
};
export default config;
