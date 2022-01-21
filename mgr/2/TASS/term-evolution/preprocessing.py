import re
from typing import Iterable, Iterator, List, Tuple

import spacy
from spacy import Language
from spacy.tokens import Token
from tqdm.auto import tqdm

SPACY_EXCLUDED_PIPELINES = ['parser', 'ner', 'entity_linker', 'entity_ruler',
                            'textcat', 'textcat_multilabel', 'senter',
                            'sentencizer', 'transformer']


def chunks(text: str, chunksize: int) -> Iterator[str]:
    start = 0
    while start < len(text):
        end = text.find('\n\n', start + chunksize)
        end = end if end > 0 else len(text)
        yield text[start:end]
        start = end


def clean_gutenberg(text: str) -> str:
    start = re.search(
        r"\*\*\* START OF (THE|THIS) PROJECT GUTENBERG EBOOK .* \*\*\*",
        text
    ).end()
    stop = re.search(
        r"\*\*\* END OF (THE|THIS) PROJECT GUTENBERG EBOOK .* \*\*\*",
        text
    ).start()
    return text[start:stop]


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
           token.like_email or \
           token.pos_ in {'PROPN', 'SYM'}


def tokenize_str(
        nlp: Language,
        text: str,
        chunksize: int = 1000,
        progress: bool = True
) -> List[str]:
    return [
        token.lemma_
        for doc in tqdm(
            nlp.pipe(chunks(text, chunksize)),
            disable=not progress
        )
        for token in doc
        if not is_irrelevant(token)
    ]


def tokenize(
        texts: Iterable[str],
        model: str = 'en_core_web_sm',
        chunksize: int = 1000,
        progress: bool = True
) -> Tuple[List[List[str]], List[str]]:
    nlp = load_spacy(model)
    tokens = [
        tokenize_str(nlp, text, chunksize, progress)
        for text in tqdm(texts, disable=not progress)
    ]
    return tokens, sorted(set(token for tkns in tokens for token in tkns))
