Todos os arquivos tem formato TSV (tab-separated values).

Todos os valores numéricos não inteiros usam vírgula como separador decimal.

## Gramáticas
Elementos `LINHA` ou começando com `LINHA-` representam uma linha inteira no arquivo.
* `TAB` - tabulação
* `X?` - `X` ocorre zero ou uma vez
* `X*` - `X` ocorre zero ou mais vezes
* `X:s` - `X` é um string (não pode conter tabulação)
* `X:i` - `X` é um número inteiro sem sinal
* `X:n` - `X` é um número sem sinal
* `X:sn` - `X` é um número possivelmente com sinal

# Padronização de Nomes
**Caminho:** data/names.tsv

Nomes encontrados em notas de corretagem são substituídos pelo nome padrão. 

```
CONTEÚDO := LINHA*
LINHA := NOME-PADRÃO:s ([TAB] OUTRO-NOME:s)*
```

# Tipos
**Caminho:** data/types.tsv

Classifica o tipo dos ativos. Ativos não listados no arquivo são considerados ações comuns ou units.

```
CONTEÚDO := LINHA*
LINHA := NOME:s [TAB] TIPO
TIPO := 'ETF' /* ETF de renda variável */
      | 'ETFRF' /* ETF de renda fixa */
      | 'FII' /* fundo imobiliário */
```

# Nota de Corretagem
**Caminho:** data/`ANO`/`DATA` - `CORRETORA`.tsv

```
CONTEÚDO :=
    LINHA-DE-NEGOCIAÇÃO*
    LINHA-EM-BRANCO
    LINHA-DE-CUSTO*
    LINHA-EM-BRANCO
    LINHA-DE-IRRF
    LINHA-DE-VALOR-DA-NOTA
LINHA-DE-NEGOCIAÇÃO := OPERAÇÃO [TAB] NOME:s [TAB] QUANTIA:i [TAB] PREÇO:n
OPERAÇÃO := 'C' /* compra */ | 'V' /* venda */
LINHA-DE-CUSTO := DESCRIÇÃO:s /* ignorada */ [TAB] CUSTO:n
LINHA-DE-IRRF := IRRF:n /* conforme aparece na nota. Pode não ter sido realmente cobrado. */
LINHA-DE-VALOR-DA-NOTA := VALOR:sn /* positivo se houve mais vendas, negativo se houve mais compras */
```

# Eventos
**Caminho:** data/`ANO`/`DATA` - EVENTS.tsv

```
CONTEÚDO := LINHA*
LINHA := DE [TAB] '->' [TAB] PARA
DE := QUANTIA:i [TAB] NOME:s
PARA := QUANTIA:i [TAB] NOME:s [TAB] PREÇO-MÉDIO ([TAB] '+' [TAB] QUANTIA:i [TAB] NOME:s [TAB] PREÇO-MÉDIO)*
PREÇO-MÉDIO := PREÇO:n
             | PORCENTAGEM:n '%'
             | MULTIPLICADOR:n 'x'
             | NUMERADOR:i '/' DENOMINADOR:i
```

## Exemplos
### Desdobramento (_Split_)
Desdobramento de 1 para 4:
```
1[TAB]MGLU3[TAB]->[TAB]4[TAB]MGLU3[TAB]1/4
```
Cada `1` ação `MGLU3` com preço médio `pm` é substituída por `4` ações `MGLU3` com preço médio de `1/4` de `pm`.

### Grupamento (_Inplit_)
Grupamento de 4 para 1:
```
4[TAB]ABCD3[TAB]->[TAB]1[TAB]ABCD3[TAB]4x
```
Cada `4` ações `ABCD3` com preço médio `pm` é substituída por `1` ação `ABCD3` com preço médio de `4` vezes `pm`.

### Bonificação
Bonificação de 1 ação de valor R$ 12,34 para cada 10 ações:
```
10[TAB]ABCD3[TAB]->[TAB]10[TAB]ABCD3[TAB]1x[TAB]+[TAB]1[TAB]ABCD[TAB]12,34
```
Cada `10` ações `ABCD3` com preço médio `pm` é substituída por `10` ações `ABCD` com o mesmo preço médio `pm`
**mais** `1` ação `ABCD3` com preço médio de `R$ 12,34`.

### Cisão
```
1[TAB]PCAR3[TAB]->[TAB]1[TAB]PCAR3[TAB]82,3%[TAB]+[TAB]1[TAB]ASAI3[TAB]17,7%
```
Cada `1` ação `PCAR3` com preço médio `pm` é substituída por `1` ação `PCAR3` com preço médio de `82,3%` de `pm`
**mais** `1` ação `ASAI3` com preço médio de `17,7%` de `pm`.

# Bens
**Gerado automaticamente!**

**Caminho:** data/`ANO`/END - assets.tsv

Representa os bens no fim do ano.
```
CONTEÚDO := LINHA*
LINHA := NOME:s [TAB] QUANTIA:i [TAB] PREÇO-MÉDIO:n [TAB] CORRETORA:s
```
