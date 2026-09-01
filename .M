// =========================================================================
// PADRONIZAÇÃO E OTIMIZAÇÃO DE TABELAS — POWER QUERY (M)   [versão refatorada]
// -------------------------------------------------------------------------
// Cole no Editor Avançado de uma Query em branco. Depois chame
// fnPadronizarTabela(SuaTabela) dentro de qualquer outra query.
//
// COMPORTAMENTO PRINCIPAL:
//   - Padroniza nomes de coluna (sem acento, MAIÚSCULO, separador "_").
//   - TODO valor em branco / só espaços / vazio VIRA null (em qualquer
//     coluna) — importante p/ leitura no banco. As linhas NÃO são removidas:
//     uma linha 100% vazia é mantida com todas as células null. (Se quiser
//     removê-las, use o parâmetro RemoverLinhasVazias = true.)
//   - Colunas com relação a DATA (nome contém DATA/ANO por padrão) são
//     convertidas para tipo date. Ano isolado (ex.: 2022) vira 01/01/2022.
//   - Colunas identificadoras (CPF, INEP, CONTRATO, etc.) ficam como texto.
//   - Demais colunas têm o tipo detectado automaticamente (número/data/texto).
//
// PARÂMETROS de fnPadronizarTabela:
//   Tabela              (obrigatório) tabela de origem
//   RemoverDuplicatas   (opcional, default false) Table.Distinct no fim
//   ColunasTextoForcado (opcional) trechos de nome que forçam TEXTO
//                       (default = ColunasTextoPadrao)
//   TamanhoAmostra      (opcional, default 1000) nº de valores amostrados p/
//                       detectar o tipo. Passe 0 para checar a coluna inteira
//   Cultura             (opcional, default "pt-BR")
//   ColunasDataForcado  (opcional) trechos de nome (por token) que forçam DATA
//                       (default = ColunasDataPadrao)
//   RemoverLinhasVazias (opcional, default false) se true, remove linhas que
//                       ficaram 100% null
// =========================================================================
let

    // ---------- Configuração padrão ----------
    // Nome de coluna que contém estes trechos => SEMPRE texto
    ColunasTextoPadrao = {
        "CPF", "CNPJ", "RG", "PIS", "NIS", "CEP",
        "TELEFONE", "FONE", "CELULAR",
        "MATRICULA", "INEP", "CODIGO", "CONTRATO", "IDENTIFICADOR"
    },

    // Token de nome de coluna que começa com estes trechos => SEMPRE data.
    // Ex.: DATA_NASCIMENTO, ANO_REFERENCIA, PERIODO_ANO_LETIVO.
    // (usa token separado por "_" p/ evitar falso-positivo tipo "PLANO")
    ColunasDataPadrao = { "DATA", "ANO", "DT" },

    // ---------- Helpers de texto ----------
    fnRemoverAcentos = (Texto as nullable text) as nullable text =>
        if Texto = null then null else
        let
            De   = {"Á","À","Â","Ã","Ä","É","È","Ê","Ë","Í","Ì","Î","Ï",
                    "Ó","Ò","Ô","Õ","Ö","Ú","Ù","Û","Ü","Ç","Ñ","Ý"},
            Para = {"A","A","A","A","A","E","E","E","E","I","I","I","I",
                    "O","O","O","O","O","U","U","U","U","C","N","Y"}
        in
            List.Accumulate(
                List.Positions(De),
                Text.Upper(Texto),
                (Estado, i) => Text.Replace(Estado, De{i}, Para{i})
            ),

    // Qualquer branco / só espaços / vazio -> null (preserva o valor original
    // se houver conteúdo). Base da regra "branco vira null" para o banco.
    fnBrancoParaNulo = (Valor as any) as any =>
        if Valor = null then null
        else if Text.Trim(Text.From(Valor)) = "" then null
        else Valor,

    // Limpa 1 valor de TEXTO: trim + colapsa espaços internos + MAIÚSCULO.
    // Vazio / só espaços / null -> null
    fnLimparTexto = (Valor as any) as nullable text =>
        if Valor = null then null else
        let
            Palavras = List.Select(Text.Split(Text.Upper(Text.From(Valor)), " "), each _ <> ""),
            Limpo    = Text.Trim(Text.Combine(Palavras, " "))
        in
            if Limpo = "" then null else Limpo,

    // Converte 1 valor para DATA de forma robusta.
    // - já é date/datetime/datetimezone -> retorna a data
    // - ano isolado "2022" (4 dígitos) -> 01/01/2022
    // - texto ISO "1985-06-02", "dd/MM/yyyy", ou serial -> data
    // - branco / não reconhecível -> null
    fnParaData = (Valor as any, Cultura as text) as nullable date =>
        if Valor = null then null
        else if Value.Is(Valor, type date) then Valor
        else if Value.Is(Valor, type datetime) then DateTime.Date(Valor)
        else if Value.Is(Valor, type datetimezone) then DateTime.Date(DateTimeZone.RemoveZone(Valor))
        else
            let
                T     = Text.Trim(Text.From(Valor)),
                EhAno = Text.Length(T) = 4 and (try Number.FromText(T, Cultura) otherwise null) <> null,
                Tentativas =
                {
                    try (let p = Text.Split(Text.Start(T, 10), "-")
                         in #date(Int64.From(p{0}), Int64.From(p{1}), Int64.From(p{2}))) otherwise null,
                    try Date.FromText(T, [Culture = Cultura]) otherwise null,
                    try Date.FromText(T) otherwise null,
                    try Date.From(Number.FromText(T, Cultura)) otherwise null
                },
                Validas = List.RemoveNulls(Tentativas)
            in
                if T = "" then null
                else if EhAno then #date(Int64.From(T), 1, 1)
                else if List.IsEmpty(Validas) then null
                else List.First(Validas),

    // Padroniza nome de coluna: sem acento, MAIÚSCULO, "_" como separador
    fnPadronizarNomeColuna = (Nome as text) as text =>
        let
            SemAcento = fnRemoverAcentos(Nome),
            Validos   = Text.Select(SemAcento, {"A".."Z", "0".."9", " ", "_"}),
            Juntado   = Text.Combine(List.Select(Text.Split(Validos, " "), each _ <> ""), "_"),
            Final     = Text.Trim(Juntado, "_")
        in
            if Final = "" then "COLUNA" else Final,

    // Coluna forçada a TEXTO? (trecho em qualquer parte do nome)
    fnForcarTexto = (NomeColuna as text, Lista as list) as logical =>
        let Alvo = Text.Upper(NomeColuna)
        in  List.AnyTrue(List.Transform(Lista, each Text.Contains(Alvo, _))),

    // Coluna forçada a DATA? (algum token "_" começa com o trecho)
    fnColunaEhData = (NomeColuna as text, Lista as list) as logical =>
        let Tokens = Text.Split(Text.Upper(NomeColuna), "_")
        in  List.AnyTrue(List.Transform(Tokens, (tok) =>
                List.AnyTrue(List.Transform(Lista, (k) => Text.StartsWith(tok, k))))),

    // ---------- Detecção de tipo (colunas "AUTO") ----------
    // number / date / datetime / text. Só classifica se TODOS os valores da
    // amostra converterem na cultura informada (evita erro de conversão).
    fnDetectarTipo = (Valores as list, Cultura as text, TamAmostra as number) as type =>
        let
            Texto = List.Select(
                List.Transform(List.RemoveNulls(Valores), each Text.Trim(Text.From(_))),
                each _ <> ""
            ),
            Amostra = if TamAmostra <= 0 then Texto else List.FirstN(Texto, TamAmostra),
            Qtd     = List.Count(Amostra),

            EhNumero   = Qtd > 0 and List.AllTrue(List.Transform(Amostra,
                            each (try Number.FromText(_, Cultura) otherwise null) <> null)),
            EhData     = Qtd > 0 and List.AllTrue(List.Transform(Amostra,
                            each (try Date.FromText(_, [Culture = Cultura]) otherwise null) <> null)),
            EhDataHora = Qtd > 0 and List.AllTrue(List.Transform(Amostra,
                            each (try DateTime.FromText(_, [Culture = Cultura]) otherwise null) <> null))
        in
            if EhNumero then type number
            else if EhData then type date
            else if EhDataHora then type datetime
            else type text,

    // ---------- Função principal ----------
    fnPadronizarTabela = (
        Tabela as table,
        optional RemoverDuplicatas as nullable logical,
        optional ColunasTextoForcado as nullable list,
        optional TamanhoAmostra as nullable number,
        optional Cultura as nullable text,
        optional ColunasDataForcado as nullable list,
        optional RemoverLinhasVazias as nullable logical
    ) as table =>
        let
            // Defaults (estilo explícito, compatível com PQ antigo)
            OpRemoverDup  = if RemoverDuplicatas    = null then false             else RemoverDuplicatas,
            ListaTexto    = if ColunasTextoForcado  = null then ColunasTextoPadrao else ColunasTextoForcado,
            TamDet        = if TamanhoAmostra       = null then 1000              else TamanhoAmostra,
            Cult          = if Cultura              = null then "pt-BR"           else Cultura,
            ListaData     = if ColunasDataForcado   = null then ColunasDataPadrao  else ColunasDataForcado,
            OpRemoverVazia= if RemoverLinhasVazias  = null then false             else RemoverLinhasVazias,

            // 1) Padroniza nomes de coluna (resolvendo colisões: X, X_2, X_3...)
            NomesOrig  = Table.ColumnNames(Tabela),
            NomesNovos = List.Accumulate(NomesOrig, {}, (Acc, NomeAtual) =>
                let
                    Base = fnPadronizarNomeColuna(NomeAtual),
                    Rep  = List.Count(List.Select(Acc, each _ = Base)),
                    Novo = if Rep = 0 then Base else Base & "_" & Text.From(Rep + 1)
                in
                    Acc & {Novo}
            ),
            Renomeada = Table.RenameColumns(Tabela, List.Zip({NomesOrig, NomesNovos})),

            // 2) Classifica cada coluna: DATA (forçada) | TEXTO | AUTO(detectado)
            Colunas = Table.ColumnNames(Renomeada),
            Info = List.Transform(Colunas, each
                let
                    Nome = _,
                    Rec =
                        if fnColunaEhData(Nome, ListaData) then
                            [Coluna = Nome, Modo = "DATA", Tipo = type date]
                        else if fnForcarTexto(Nome, ListaTexto) then
                            [Coluna = Nome, Modo = "TEXTO", Tipo = type text]
                        else
                            [Coluna = Nome, Modo = "AUTO",
                             Tipo = fnDetectarTipo(Table.Column(Renomeada, Nome), Cult, TamDet)]
                in
                    Rec
            ),

            // 3) UMA passada de transformação de valores:
            //    - DATA forçada  -> fnParaData
            //    - TEXTO         -> fnLimparTexto (já transforma branco em null)
            //    - AUTO não-texto-> fnBrancoParaNulo (branco->null antes de tipar)
            TransfData = List.Transform(
                List.Select(Info, each [Modo] = "DATA"),
                each {[Coluna], (v) => fnParaData(v, Cult), type date}
            ),
            TransfTexto = List.Transform(
                List.Select(Info, each [Tipo] = type text),   // TEXTO forçado + AUTO-texto
                each {[Coluna], fnLimparTexto, type text}
            ),
            TransfNulo = List.Transform(
                List.Select(Info, each [Modo] = "AUTO" and [Tipo] <> type text),
                each {[Coluna], fnBrancoParaNulo}
            ),
            TransfValores = TransfData & TransfTexto & TransfNulo,
            TabelaValores = if List.IsEmpty(TransfValores)
                            then Renomeada
                            else Table.TransformColumns(Renomeada, TransfValores),

            // 4) UMA passada de conversão de tipo (colunas AUTO número/data/hora)
            TransfTipos = List.Transform(
                List.Select(Info, each [Modo] = "AUTO" and [Tipo] <> type text),
                each {[Coluna], [Tipo]}
            ),
            TabelaTipada = if List.IsEmpty(TransfTipos)
                           then TabelaValores
                           else Table.TransformColumnTypes(TabelaValores, TransfTipos, Cult),

            // 5) (opcional) remove linhas que ficaram 100% null
            SemVazias =
                if OpRemoverVazia then
                    Table.SelectRows(TabelaTipada, each
                        List.AnyTrue(List.Transform(Record.FieldValues(_), (v) => v <> null)))
                else
                    TabelaTipada,

            // 6) (opcional) duplicatas
            Final = if OpRemoverDup then Table.Distinct(SemVazias) else SemVazias
        in
            Final

in
    fnPadronizarTabela


// =========================================================================
// EXEMPLO DE USO (em outra query, apontando para a função acima)
// =========================================================================
// let
//     Origem      = Csv.Document(File.Contents("C:\Caminho\Arquivo.csv"), [Delimiter=";", Encoding=1252]),
//     Cabecalho   = Table.PromoteHeaders(Origem, [PromoteAllScalars=true]),
//
//     // uso simples (defaults):
//     Padronizado = fnPadronizarTabela(Cabecalho)
//
//     // uso completo:
//     // Padronizado = fnPadronizarTabela(
//     //     Cabecalho,
//     //     true,                          // remover duplicatas
//     //     {"CPF","INEP","CONTRATO"},     // colunas forçadas a texto
//     //     0,                             // 0 = checar coluna inteira p/ detectar tipo
//     //     "pt-BR",
//     //     {"DATA","ANO","VENCIMENTO"},   // trechos que forçam DATA
//     //     false                          // manter linhas vazias (só vira null)
//     // )
// in
//     Padronizado
