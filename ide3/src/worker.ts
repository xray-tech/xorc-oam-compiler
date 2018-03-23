class Worker {

}

export function create(ctx: any, createData: any) {
    console.log("---NEW WORKER", ctx, createData)
    return new Worker();
}
