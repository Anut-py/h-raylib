#include "bindings.h"

void testing(int num) {
    InitWindow(600, 450, "Testing window");

    SetTargetFPS(60);

    while (!WindowShouldClose()) {
        BeginDrawing();

        ClearBackground(RAYWHITE);
        DrawText("Got number: " + num, 190, 200, 20, GRAY);

        EndDrawing();
    }

    CloseWindow();
}