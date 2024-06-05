## Descrição do Código

Este código ABAP contém várias formas (sub-rotinas) para selecionar, processar e formatar dados de materiais e estoques. O objetivo principal é extrair informações de várias tabelas relacionadas a materiais (como MARD, MARA, MAK, T001W, T001L, e MCHB), processar esses dados e preparar saídas formatadas para diferentes tipos de relatórios.

### Estrutura Principal do Código

O código é organizado em várias formas, cada uma com um propósito específico:

1. **`yf_seleciona_dados_lote_false`**: Seleciona dados de materiais e estoques sem considerar lotes.
2. **`yf_seleciona_dados_lote_true`**: Seleciona dados de materiais e estoques considerando lotes.
3. **`yf_processa_dados_lote_false`**: Processa os dados selecionados para materiais e estoques sem considerar lotes.
4. **`yf_processa_dados_lote_true`**: Processa os dados selecionados para materiais e estoques considerando lotes.
5. **`yf_processa_saida_lote_false`**: Processa e formata a saída dos dados sem considerar lotes.
6. **`yf_processa_saida_lote_true`**: Processa e formata a saída dos dados considerando lotes.
7. **`yf_convert_quant`**: Converte unidades de medida dos materiais.
8. **`yf_monta_filedcat`**: Monta a estrutura de campo para os relatórios ALV (ABAP List Viewer).

### Descrição Detalhada das Formas

#### `yf_seleciona_dados_lote_false`

Esta forma seleciona dados de materiais e estoques das tabelas MARD, MARA, MAK e T001W. A seleção é baseada nos parâmetros `sc_werks`, `sc_matnr` e `sc_lgort`.

#### `yf_seleciona_dados_lote_true`

Semelhante à forma anterior, mas inclui a seleção de dados da tabela MCHB, que contém informações sobre lotes.

#### `yf_processa_dados_lote_false`

Processa os dados selecionados para materiais e estoques sem considerar lotes. Realiza leituras nas tabelas MAK, T001W, MARA, e opcionalmente na MCHB e T001L se `pc_edn` estiver inicializado.

#### `yf_processa_dados_lote_true`

Processa os dados selecionados para materiais e estoques considerando lotes. Realiza leituras nas tabelas MAK, T001W, T001L e MARA.

#### `yf_processa_saida_lote_false`

Processa e formata a saída dos dados sem considerar lotes. Inclui cálculos de saldo disponível (`estoq_disp`) e conversões de unidades de medida.

#### `yf_processa_saida_lote_true`

Processa e formata a saída dos dados considerando lotes. Inclui a formatação do valor dos materiais e tratamento de saldos nulos.

#### `yf_convert_quant`

Converte unidades de medida dos materiais utilizando a função `MD_CONVERT_MATERIAL_UNIT`.

#### `yf_monta_filedcat`

Monta a estrutura de campos para os relatórios ALV. Define quais campos serão exibidos nos relatórios, tanto para cenários com lotes desativados (`pc_lote` inicial) quanto para cenários com lotes ativados.

### Tabelas Utilizadas

- **MARD**: Estoques por depósito
- **MARA**: Dados gerais de materiais
- **MAKT**: Textos de materiais
- **T001W**: Centros (plantas)
- **T001L**: Depósitos
- **MCHB**: Estoques por lote
- **S094**: Estrutura para saldos futuros