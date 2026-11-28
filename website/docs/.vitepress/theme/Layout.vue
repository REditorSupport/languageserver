<script setup>
import DefaultTheme from 'vitepress/theme'
import { withBase } from 'vitepress'
</script>

<template>
  <DefaultTheme.Layout>
    <template #home-hero-image>
      <figure class="signature-example">
        <figcaption>center.R</figcaption>
        <pre><code>center &lt;- <strong>function</strong>(values) {
  values - mean(values)
}

center(<span class="caret" aria-hidden="true"></span></code></pre>
        <div class="signature-help">
          <span>Signature Help</span>
          <code>center(<b>values</b>)</code>
        </div>
        <a :href="withBase('/guide/quick-start')">Try the five-minute walkthrough <span aria-hidden="true">→</span></a>
      </figure>
    </template>
  </DefaultTheme.Layout>
</template>
