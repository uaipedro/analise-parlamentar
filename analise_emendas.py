import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import unicodedata

def setup_visualization():
    plt.style.use('seaborn-v0_8-darkgrid')
    pd.set_option('display.max_columns', None)

def load_dataframes():
    try:
        amendments = pd.read_csv('data/emendas.csv', encoding='latin1', sep=';', low_memory=False)
        deputies = pd.read_csv('data/deputados_atuais.csv', encoding='utf-8', sep=';')
        
      
        amendments['Valor Empenhado'] = amendments['Valor Empenhado'].str.replace('R$', '')\
            .str.replace('.', '')\
            .str.replace(',', '.')\
            .astype(float)
        
        amendments['Ano'] = amendments['Ano da Emenda']
            
        return amendments, deputies
        
    except FileNotFoundError as e:
        print(f"Arquivo não encontrado: {e}")
        return None, None
    except Exception as e:
        print(f"Erro ao carregar dados: {e}")
        return None, None

def normalize_name(name):
    if pd.isna(name):
        return name
    # Normaliza caracteres especiais (ex: á -> a) e converte para ASCII puro,
    normalized = unicodedata.normalize('NFKD', str(name)).encode('ASCII', 'ignore').decode('ASCII')
    normalized = ' '.join(normalized.split()).lower()
    return normalized

def merge_dataframes(amendments, deputies):
    amendments_clean = amendments.copy()
    deputies_clean = deputies.copy()
    
    amendments_clean['nome_normalizado'] = amendments_clean['Nome do Autor da Emenda'].apply(normalize_name)
    deputies_clean['nome_normalizado'] = deputies_clean['Nome Parlamentar'].apply(normalize_name)
    
    print("\n=== Dados antes do merge ===")
    print(f"Valores únicos em 'Nome do Autor da Emenda':\n{amendments_clean['nome_normalizado'].nunique()}")
    print(f"Valores únicos em 'Nome Parlamentar':\n{deputies_clean['nome_normalizado'].nunique()}")
    
    print("\nAmostras de nomes normalizados:")
    print("Emendas:", amendments_clean[['Nome do Autor da Emenda', 'nome_normalizado']].head())
    print("Deputados:", deputies_clean[['Nome Parlamentar', 'nome_normalizado']].head())
    
    try:
        political_spectrum = pd.read_csv('data/partidos2024.csv', sep=';')
    except FileNotFoundError:
        political_spectrum = pd.read_csv('data/partidos2020.csv')
    
    merged_df = amendments_clean.merge(
        deputies_clean[['nome_normalizado', 'Partido']],
        on='nome_normalizado',
        how='left'
    ).drop('nome_normalizado', axis=1)
    
    merged_df = merged_df.merge(
        political_spectrum[['SG_PARTIDO', 'Espectro']],
        left_on='Partido',
        right_on='SG_PARTIDO',
        how='left'
    ).drop('SG_PARTIDO', axis=1)
    
    print(f"\nLinhas antes do merge: {len(amendments)}")
    print(f"Linhas após o merge: {len(merged_df)}")
    print(f"Registros sem partido: {merged_df['Partido'].isna().sum()}")
    print(f"Registros sem espectro político: {merged_df['Espectro'].isna().sum()}")
    
    return merged_df

def format_currency(value):
    return f"R$ {value:,.2f}".replace(',', '_').replace('.', ',').replace('_', '.')

def generate_party_summary(df):
    summary = df.groupby(['Partido', 'Espectro']).agg({
        'Valor Empenhado': ['sum', 'count', 'mean']
    })
    
    summary.columns = ['Total Empenhado', 'Quantidade de Emendas', 'Média por Emenda']
    summary = summary.sort_values('Total Empenhado', ascending=False)
    
    summary['Total Empenhado'] = summary['Total Empenhado'].apply(format_currency)
    summary['Média por Emenda'] = summary['Média por Emenda'].apply(format_currency)
    return summary

def plot_party_analysis(df, suffix=''):
    def format_billions(x, pos):
        return f'R${x/1e9:.1f}B'
    
    def format_millions(x, pos):
        return f'R${x/1e6:.1f}M'
    
    fig = plt.figure(figsize=(15, 10))
    
    # Gráfico de barras - Total por partido
    ax1 = plt.subplot(2, 2, 1)
    party_totals = df.groupby('Partido')['Valor Empenhado'].sum().sort_values(ascending=True)
    ax1.barh(range(len(party_totals)), party_totals.values)
    ax1.set_yticks(range(len(party_totals)))
    ax1.set_yticklabels(party_totals.index)
    ax1.xaxis.set_major_formatter(plt.FuncFormatter(format_billions))
    plt.title('Valor Total Empenhado por Partido')
    
    # Gráfico de pizza - Distribuição por valor
    ax2 = plt.subplot(2, 2, 2)
    valor_por_partido = df.groupby('Partido')['Valor Empenhado'].sum()
    ax2.pie(valor_por_partido.values, labels=valor_por_partido.index, autopct='%1.1f%%')
    plt.title('Distribuição do Valor Empenhado por Partido')
    
    # Boxplot com escala logarítmica
    ax3 = plt.subplot(2, 1, 2)
    sns.boxplot(data=df, x='Partido', y='Valor Empenhado', showfliers=False)
    ax3.set_yscale('log')
    plt.xticks(rotation=45)
    plt.title('Distribuição dos Valores por Partido (Escala Log)')
    ax3.yaxis.set_major_formatter(plt.FuncFormatter(format_millions))
    
    plt.tight_layout()
    plt.savefig(f'plots/analise_partidos{suffix}.png', dpi=300, bbox_inches='tight')
    plt.close()

def plot_spectrum_analysis(df, suffix=''):
    def format_billions(x, pos):
        return f'R${x/1e9:.1f}B'
    
    spectrum_colors = {
        'esquerda': '#FF0000',  # Vermelho
        'centro': '#808080',    # Cinza
        'direita': '#0000FF'    # Azul
    }
    
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(15, 6))
    
    # Filtra apenas os dados com espectro definido
    df_spectrum = df[df['Espectro'].notna()]
    
    spectrum_totals = df_spectrum.groupby('Espectro')['Valor Empenhado'].sum().sort_values(ascending=True)
    spectrum_totals.plot(kind='barh', ax=ax1, color=[spectrum_colors[s] for s in spectrum_totals.index])
    ax1.xaxis.set_major_formatter(plt.FuncFormatter(format_billions))
    ax1.set_title('Valor Total Empenhado por Espectro Político')
    
    valor_por_espectro = df_spectrum.groupby('Espectro')['Valor Empenhado'].sum()
    valor_por_espectro.plot(kind='pie', autopct='%1.1f%%', ax=ax2, 
                           colors=[spectrum_colors[s] for s in valor_por_espectro.index])
    ax2.set_title('Distribuição do Valor Empenhado por Espectro Político')
    
    plt.tight_layout()
    plt.savefig(f'plots/analise_espectro{suffix}.png', dpi=300, bbox_inches='tight')
    plt.close()

def display_summary(df):
    print("\n=== RESUMO DA ANÁLISE GERAL ===")
    anos = sorted(df['Ano'].unique())
    
    for ano in anos:
        if ano < 2020:
            continue
            
        df_ano = df[df['Ano'] == ano]
        print(f"\n\n=== ANÁLISE DO ANO {ano} ===")
        print(f"Total de emendas: {len(df_ano):,}")
        print(f"Total de deputados únicos: {df_ano['Nome do Autor da Emenda'].nunique():,}")
        
        valor_total = format_currency(df_ano['Valor Empenhado'].sum())
        valor_medio = format_currency(df_ano['Valor Empenhado'].mean())
        
        print(f"\nValor total empenhado: {valor_total}")
        print(f"Valor médio por emenda: {valor_medio}")
        
        print(f"\nDistribuição por partido e espectro político:")
        print(generate_party_summary(df_ano))
        
        plot_party_analysis(df_ano, suffix=f"_{ano}")
        plot_spectrum_analysis(df_ano, suffix=f"_{ano}")

def main():
    setup_visualization()
    
    amendments, deputies = load_dataframes()
    if amendments is None or deputies is None:
        return
    
    complete_df = merge_dataframes(amendments, deputies)
    display_summary(complete_df)
    complete_df.to_csv('amendments_with_parties.csv', index=False)

if __name__ == "__main__":
    main() 