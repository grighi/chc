

from playwright.sync_api import sync_playwright
import shlex

def request_to_curl(request):
    cmd = ["curl", request.url]
    
    # method
    if request.method != "GET":
        cmd += ["-X", request.method]
    
    # headers
    for k, v in request.headers.items():
        if k.lower() != "content-length":
            cmd += ["-H", f"{k}: {v}"]
    
    # post data
    if request.post_data:
        cmd += ["--data-raw", request.post_data]
    
    return " ".join(shlex.quote(c) for c in cmd)

with sync_playwright() as p:
    browser = p.chromium.launch(headless=False)  # set True later
    page = browser.new_page()

    # Capture network requests
    def handle_request(request):
        if "datarequest/D77" in request.url and request.method == "POST":
            curl_cmd = request_to_curl(request)
            with open("wonder_post.curl.txt", "w") as f:
                f.write(curl_cmd)
            print("Captured POST as curl!")

    page.on("request", handle_request)

    # Step 1: open agreement page
    page.goto("https://wonder.cdc.gov/mcd-icd10.html")

    # Step 2: click "I Agree"
    page.click('input[value="I Agree"]')

    # Wait for the real submit button to be ready
    page.wait_for_selector("#submit-button1", state="visible")

    # Extra safety: ensure JS is fully attached
    page.wait_for_timeout(2000)

    # Scroll into view (important for legacy forms)
    page.locator("#submit-button1").scroll_into_view_if_needed()

    # Click it
    page.locator("#submit-button1").click()

