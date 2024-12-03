# eae2 - Estatística Aplicada à Epidemiologia 2

Repositorio com conteúdo da disciplina Estatística Aplicada à Epidemiologia 2.

Neste repositório ficarão depositados aulas e códigos em R utilizados na disciplina Estatística Aplicada à Epidemiologia 2, ministrada na ENSP/Fiocruz.

## Slides (handouts)

Não recomendo que estudem pelos slides, por isso indico pelo menos um capítulo de livro que usei para preparar os slides. Não se prendam aos livros indicados, se tiver outro e estiver na dúvida quanto a qualidade pode me procurar.

| Data     | Slides                                                                       | Capítulo sugerido              |
|-------------------|---------------------------------|--------------------|
| 6/8/2024 | [Burocracias](slides/0_burocracias.pdf) e [Introdução](slides/1_intro.pdf)   | Woodward 9.1                   |
| 13/8     | Greve                                                                        | \-                             |
| 20/8     | Greve                                                                        | \-                             |
| 27/8     | [Modelos lineares 1](slides/2_lm.pdf)                                        | Woodward 9.2-9.4               |
| 3/9      | [Modelos lineares 2](slides/3_lm.pdf); [R](pratica/3_lm/)                                      | Woodward 9.5-9.8               |
| 10/9     | Jornada acadêmica                                                            | \-                             |
| 17/9     | [Modelos lineares 3](slides/4_lm.pdf); ([Pratica lm](pratica/2_lm/)); [Introdução ao modelo logístico ](slides/4_Logistic.pdf); [R](pratica/4_lm/) | Jewell Cap. 12               |
| 24/9     | [Modelo logístico I](slides/5_Logistic.pdf);  [R](pratica/5_logistic/)| Jewell Cap 13; Woodward Cap 10 |
| 1/10     | [Modelo logístico II](slides/6_Logistic.pdf);  [R](pratica/6_logistic/)                                                         | Jewell Cap 13; Woodward Cap 10 |
| 8/10     | [Avaliação I](avaliacao/) (pdf e RData)     | \-                             |
| 15/10    | [Confundimento e Interação I](slides/7_confund.pdf) [R](pratica/7_confund/) | Woodward Cap 14    |
| 22/10    | [Inferência em amostras complexas](slides/8_amostragem.pdf) e [mau uso do valor-p](slides/8_Bastos_palestra_IOC_p-value.pdf) | [Survey](http://r-survey.r-forge.r-project.org/survey/) e [ASA statment on p-values](https://www.tandfonline.com/doi/full/10.1080/00031305.2016.1154108#d1e385); [R](pratica/8_amostragem/amostragem.R)  |
| 29/10    |  Aula cancelada  |                                |
| 5/11     | Aula prática     |                                |
| 12/11    | [Análise de mediação](slides/9_mediacao.pdf)      |  [VanderWeele's paper](https://pubmed.ncbi.nlm.nih.gov/26653405/) |
| 19/11    | Ponto facultativo no Rio - G20                                               | \*                             |
| 26/11    | Não tem aula (Congresso de epidemiologia); Data limite do envio do tótulo e resumo da avaliação II | \*                             |
| 3/12     | Avaliação II: Apresentações de ~~10~~15 minutos por grupo. ~~Envio da avaliação do projeto do colega~~ | [Resumos](avaliacao/)   |
| 4/12     | Avaliação II: Data limite para o envio ~~do resumo atualizado~~ da avaliação do trabalho do(s) colega(s). |  [Alocação](avaliacao/alocacao.csv)                              |

## Práticas e exercícios

| Data      | Pdf, dados e código em R   |
|-----------|----------------------------|
| 3/9/2024  | [Pratica 1](pratica/1_lm/) |
| 17/9/2024 | [Pratica 2](pratica/2_lm/) |
| 24/9/2024 | [Pratica 3](pratica/5_logistic/) |
| 1/10/2024 | [Pratica 4](pratica/6_logistic/) |
| 15/10/2024 | [Pratica 5](pratica/7_confund/) |
| 5/11/2024 | [Pratica 6](pratica/8_amostragem/) |
| 12/11/2024 | [Pratica 7](pratica/9_mediacao/) |

## Trabalho final

O trabalho final da disciplina consiste na análise de uma base de dados real e aberta, a escolha do grupo, com desfecho binário, uma exposição de interesse e pelo menos duas outras variáveis para avaliar a presença de confundimento e interação. O trabalho consiste em 3 etapas:

1) Até dia *26/11* cada grupo deverá enviar um resumo estruturado, podendo ser tanto em inglês ou quanto em português, de até 250 palavras com introdução (ou backround ou objetivos), métodos, resultados e conclusão. Revistas de exemplo [Epidemiology](https://journals.lww.com/epidem/) ou [International Journal of Epidemiology](https://academic.oup.com/ije/issue/53/5). Segue um link com [uma nota do editor](https://journals.lww.com/epidem/blog/watching/pages/post.aspx?PostID=20) da Epidemiology e um link para [o guideline da IJE](https://academic.oup.com/ije/pages/General_Instructions#Original%20Articles) sobre abstracts.
2) No dia *3/12* teremos apresentações de ~~10~~ até 15 minutos de cada trabalho.
~~3) Após apresentação, os resumos poderão ser atualizados e reenviados até dia *4/12*.~~

Cada aluno será alocado um resumo para avaliação ~~, podendo ou não fazer comentários breves na apresentação do colegas~~. Uma breve avaliação escrita do trabalho do colega com no máximo 300 caracteres e uma nota de 0 a 10 para o trabalho a serem enviadas por email ~~ainda no dia 3/12~~ até 4/12.

Proposta de título do trabalho: Associação entre "desfecho Y" e "exposição X" na "populaçao Z". Ex. Associação entre baixo peso ao nascer e raça/cor da mãe no Rio de Janeiro, Brasil.

A avaliação será feita com base nos resumos enviados, na apresentação, na avaliação do trabalho feita pelo colega (o comentário do colega será considerado assim como a avaliação em si será avaliada).

Tanto o resumo quanto a avaliação do colega devem ser enviados em texto simples no corpo do email. 

Uso de ferramentas de inteligência artificial deve ser explicitada, se usou qual feramenta, onde usou e porque usou. (Não é problema fazer uso de IA, mas eu gostaria que fosse explícito onde ela foi utilizada).

## Bibliografia básica

### Livros

-   Woodward: Epidemiology: Study Design and Data Analysis, Third Edition [link](https://www.taylorfrancis.com/books/mono/10.1201/b16343/epidemiology-mark-woodward)

-   Jewel: Statistics for epidemiology [link](https://www.taylorfrancis.com/books/mono/10.1201/9781482286014/statistics-epidemiology-nicholas-jewell?context=ubx&refId=ab3f5834-d7f5-413b-895e-d45430b4a4c9)

-   OpenStats: OpenIntro Statistics [link](https://www.openintro.org/book/os/)

-   VanderWeele: Explanation in causal inference: methods for mediation and interaction. Oxford University press. [link](https://global.oup.com/academic/product/explanation-in-causal-inference-9780199325870?cc=us&lang=en&)


### Artigos

- Barros and Hirakata (2003) Alternatives for logistic regression in cross-sectional studies: an empirical comparison of models that directly estimate the prevalence ratio [PUBMED](https://pubmed.ncbi.nlm.nih.gov/14567763/)
- Bastos, de Oliveira and Velasque (2014) Obtaining adjusted prevalence ratios from logistic regression models in cross-sectional studies. [PUBMED](https://pubmed.ncbi.nlm.nih.gov/25859716/)
- Greenland (1989) Modeling and variable selection in epidemiologic analysis. [PUBMED](https://pubmed.ncbi.nlm.nih.gov/2916724/)
- Lange, T., et al. (2017) Applied mediation analyses: a review and tutorial. [PUBMED](https://pubmed.ncbi.nlm.nih.gov/29121709/)
- Richiardi et al. (2013) Mediation analysis in epidemiology: methods, interpretation and bias. [PUBMED](https://pubmed.ncbi.nlm.nih.gov/24019424/)
- Sperandei (2014) Understanding logistic regression analysis. [PUBMED](https://pubmed.ncbi.nlm.nih.gov/24627710/)
- Steen, J., et al. (2017) Medflex: An R Package for Flexible Mediation Analysis using Natural Effect Models. \textit{Journal of Statistical Software} [Link](https://www.jstatsoft.org/article/view/v076i11).
- VanderWeele (2015) Mediation Analysis: A Practitioner’s Guide. [PUBMED](https://pubmed.ncbi.nlm.nih.gov/26653405/) 



