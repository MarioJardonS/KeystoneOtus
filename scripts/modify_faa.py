'''
Script hecho para agregar el taxon_name a archivos .faa

Uso python modify_faa.py -i gff_file -k kraken_file -r krakenreport_file -f fasta_file

'''

import pandas as pd
from Bio import SeqIO
import os
from pathlib import Path
import argparse

parser = argparse.ArgumentParser(description= 'Script to add taxon_name to .gff files')
parser.add_argument('-i', '--input-file', required = True, nargs = 1,
                   help = 'add .gff file')
parser.add_argument('-k', '--kraken-file', required = True, nargs = 1,
                   help = 'add .kraken file')
parser.add_argument('-r', '--kraken-report', required = True, nargs = 1,
                   help = 'add .kraken report file')
parser.add_argument('-f', '--fasta-file', required = True, nargs = 1,
                   help = 'add .faa original file')
#parser.add_argument('-o', '--output', required = True, nargs = 1,
#                   help = 'output path')
args = parser.parse_args()
#print(args)

ruta_prokka="/files2/camda2024/gut/prokka/"
ruta_kraken="/files2/camda2024/gut/taxonomy/assembly/kraken/"
ruta_outputs="/files2/camda2024/gut/taxonomy/assembly/reports/"
ruta_salida="/files2/camda2024/gut/faa_files/"

input_file = os.path.join(os.getcwd(), str(args.input_file[0]))
kraken_file = os.path.join(os.getcwd(), str(args.kraken_file[0]))
kraken_report = os.path.join(os.getcwd(), str(args.kraken_report[0]))
fasta_file = os.path.join(os.getcwd(), str(args.fasta_file[0]))
#output_path = os.path.join(os.getcwd(), str(args.output[0]))
sample = Path(fasta_file).stem
#print(sample)

# Cargar el archivo GFF de Prokka
prokka_df = pd.read_csv(input_file, sep="\t", comment="#", header=None)
#print(prokka_df)
prokka_df.columns = ["contig", "source", "type", "start", "end", "score", "strand", "phase", "attributes"]

# Extraer IDs de contig y de gen/proteína
prokka_df['contig_id'] = prokka_df['attributes'].apply(lambda x: x.split(';')[0].split('=')[1])

# Función para extraer el valor de 'product' de la cadena de 'attributes'
def extract_product(attributes):
    # Dividir la cadena por ';' para separar cada par clave=valor
    attributes_list = attributes.split(';')
    # Iterar sobre cada par
    for attr in attributes_list:
        if attr.startswith('product='):
            # Extraer y retornar el valor después de 'product='
            return attr.split('=')[1]
    return "Unknown"  # Retornar "Unknown" si 'product' no se encuentra

# Aplicar la función a la columna 'attributes' y crear la nueva columna 'product'
prokka_df['product'] = prokka_df['attributes'].apply(extract_product)

# Cargar el archivo de salida de Kraken
kraken_df = pd.read_csv(kraken_file, sep="\t", header=None)
kraken_df.columns = ["classification", "seq_id", "tax_id", "seq_length", "taxonomic_assignment"]

# Cargar el archivo de mapeo de OTUs
otu_df = pd.read_csv(kraken_report, sep="\t", header=None)
otu_df.columns = ["percentage_of_total", "num_seqs_at_taxon", "num_seqs_at_taxon_incl_subtaxa", "rank_code", "tax_id", "taxon_name"]

# Mapear tax_id de Kraken a Prokka
prokka_df = prokka_df.merge(kraken_df[['seq_id', 'tax_id']], left_on='contig', right_on='seq_id', how='left')
prokka_df.drop(columns=['seq_id'], inplace=True)  # Eliminar la columna duplicada de seq_id

# Mapear taxon_name usando tax_id
prokka_df = prokka_df.merge(otu_df[['tax_id', 'taxon_name']], on='tax_id', how='left')
#taxon = prokka_df['taxon_name']

# Agrupar por 'contig_id' y combinar 'product' y 'taxon_name'
prokka_df['combined'] = prokka_df['product']+ " " + "("+prokka_df['taxon_name'].str.strip()+ ")"
grouped = prokka_df.groupby('contig_id')['combined'].apply('; '.join).reset_index()

# Crear diccionario
id_map = pd.Series(grouped.combined.values, index=grouped.contig_id).to_dict()
#print(id_map)
# Modificar descripciones en FASTA
def modify_fasta_description(records, id_map):
    for record in records:
        record_id = record.id
        description = id_map.get(record_id, "Unknown")
        record.description = f"{record_id} {description}"
        yield record


# Leer el archivo FASTA original
input_fasta_path = fasta_file
records = list(SeqIO.parse(input_fasta_path, "fasta"))

# Modificar los identificadores
modified_records = modify_fasta_description(records, id_map)

# Guardar el archivo FASTA modificado
output_fasta_path = ruta_salida+sample+"_modified.faa"
#output_fasta_path
SeqIO.write(modified_records, output_fasta_path, "fasta")
