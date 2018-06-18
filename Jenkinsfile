def label = "xray-re-${UUID.randomUUID().toString()}"
podTemplate(name: 'docker', label: label, yaml: """
apiVersion: v1
kind: Pod
metadata:
    name: dind
spec:
    containers:
      - name: docker
        image: docker/compose:1.21.2
        command: ['cat']
        tty: true
        env:
          - name: DOCKER_HOST
            value: tcp://localhost:2375
        volumeMounts:
          - name: maven
            mountPath: /root/.m2
          - name: service-account
            mountPath: /etc/service-account
      - name: dind-daemon
        image: docker:18.03.1-dind
        securityContext:
            privileged: true
        volumeMounts:
          - name: docker-graph-storage
            mountPath: /var/lib/docker
        resources:
          requests:
            cpu: 1
    volumes:
      - name: docker-graph-storage
        persistentVolumeClaim:
          claimName: ci-graph-storage
      - name: maven
        persistentVolumeClaim:
          claimName: ci-maven-storage
      - name: service-account
        secret:
          secretName: service-account
 """
   )
 {
    node(label) {
        def scmVars = checkout([
            $class: 'GitSCM',
            branches: scm.branches,
            doGenerateSubmoduleConfigurations: false,
            extensions: [[
                $class: 'SubmoduleOption',
                disableSubmodules: false,
                parentCredentials: true,
                recursiveSubmodules: true,
                reference: '',
                trackingSubmodules: false
            ]],
            submoduleCfg: [],
            userRemoteConfigs: scm.userRemoteConfigs
        ])
        def gitCommit = scmVars.GIT_COMMIT
        def image = "eu.gcr.io/xray2poc/orc:${gitCommit}"
        stage('Build image') {
            container('docker') {
                sh("docker login -u _json_key --password-stdin https://eu.gcr.io < /etc/service-account/xray2poc.json")
                sh("docker build -t ${image} re")
                sh("docker push ${image}")
            }
        }

        stage('Test') {
            container('docker') {
                sh("docker run ${image} testkit.exe exec orc.exe -- tests-server")
            }
        }

    }
}
