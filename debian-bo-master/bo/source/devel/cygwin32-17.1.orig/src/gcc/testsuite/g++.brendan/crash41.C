typedef void (**ppfn)(void);

int main() {
    ppfn fn;

    fn = new (void(*)(void));

    return 0;
}
