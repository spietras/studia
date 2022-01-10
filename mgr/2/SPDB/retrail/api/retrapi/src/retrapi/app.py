from fastapi import FastAPI

app = FastAPI(title="retrapi", description="retrail api")


@app.get("/about", response_model=str)
def get_about():
    return "retrapi"
