import os
from typing import Iterable, List, Sequence, Tuple, TypeVar

import spacy
from spacy import Language
from spacy.tokens import Token

T = TypeVar('T')

SPACY_EXCLUDED_PIPELINES = ['parser', 'ner', 'entity_linker', 'entity_ruler',
                            'textcat', 'textcat_multilabel', 'senter',
                            'sentencizer', 'tok2vec', 'transformer']


def chunks(seq: Sequence[T], chunksize: int) -> T:
    return (seq[pos:pos + chunksize] for pos in range(0, len(seq), chunksize))


def load_spacy(model: str) -> Language:
    if not spacy.util.is_package(model):
        spacy.cli.download(model, False, False, ["--quiet"])
    return spacy.load(
        model,
        exclude=SPACY_EXCLUDED_PIPELINES
    )


def is_irrelevant(token: Token) -> bool:
    return token.is_stop or \
           token.is_space or \
           token.is_punct or \
           token.is_quote or \
           token.is_bracket or \
           token.is_digit or \
           token.is_currency or \
           token.like_num or \
           token.like_url or \
           token.like_email


def tokenize_str(
        nlp: Language,
        text: str,
        chunksize: int = 20000
) -> List[str]:
    old_max_length, nlp.max_length = nlp.max_length, chunksize + 1
    tokens = [
        token.lemma_
        for doc in nlp.pipe(chunks(text, chunksize), n_process=os.cpu_count())
        for token in doc
        if not is_irrelevant(token)
    ]
    nlp.max_length = old_max_length
    return tokens


def tokenize(
        texts: Iterable[str],
        model: str = 'en_core_web_sm',
        chunksize: int = 20000
) -> Tuple[List[List[str]], List[str]]:
    nlp = load_spacy(model)
    tokens = [
        tokenize_str(nlp, text, chunksize)
        for text in texts
    ]
    return tokens, sorted(set(token for tkns in tokens for token in tkns))
